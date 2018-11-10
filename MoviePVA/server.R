# =================================================================
# COMP5703 - IT CAPSTONE PROJECT - SCALABLE MOVIE VISUAL ANALYTICS
# =================================================================
# Author: Marni Torkel
# Codes are based on tutorial from the following:
# Github, Kaggles, R Studio, Kateto.net, Stackoverflow, etc
# ================================================================
#
source("Global.R")
#library(ggplot2movies)
library(randomcoloR)
library(dplyr)
library(scales)
library(stringr)
library(fastmatch)
library(ggplot2)
library(ggTimeSeries)
library(ggthemes)
#library(streamgraph)
library(htmlwidgets)
#library(plotly)
library(bubbles)
library(wordcloud)
library(RColorBrewer)
library(NLP)
library(tm)
library(networkD3)
library(sunburstR)
library(DT)
#library(ndtv)
library(network)
library(sna)
library(networkDynamic)
library(animation)
library(ggraph)
library(ggforce)
library(gganimate)
library(igraph)
library(ggnet)
library(GGally)
library(intergraph)
library(ggrepel)
library(edgebundleR)
library(leaflet)

options(shiny.sanitize.errors = TRUE)

shinyServer(function(input, output, session) {
  # =========== OVERVIEW ============
  
  
  variablename <- reactive({
    paste0(str_to_title(gsub("_", " ", input$variable)))
  })
  
  output$caption <- renderText({
    if (input$variable == "director_name") {
      paste0("Visualisation - Top 20 Grossing ",variablename(),"s")
    }
    else if (input$variable == "movie_title") {
      paste0("Visualisation - Top 50 Grossing ",variablename(),"s")
    }
    else {
      paste0("Visualisation - ",variablename())
    }
    
  })
  
  output$varList <- renderPrint({
    num <- fmatch(input$variable, names(imdb_data))
    if (class(imdb_data[[num]]) == "character") {
      if (input$variable == "movie_title")
      {
        output$dataMes <- renderText({
          return("Top revenue movies")
        })
        
        title_rev <- data.frame(imdb_data$movie_title, round(imdb_data$gross/1000000,0))
        title_rev <- unique(title_rev[order(-title_rev[,2]),])
        colnames(title_rev) <- c("Title", "Gross(m)")
        rownames(title_rev) <- NULL
        return(title_rev[1:4,])
      }
      else if (input$variable == "director_name") {
        output$dataMes <- renderText({
          return("Top directors - avg revenue")
        })
        dir_rev <- data.frame(imdb_data$director_name, round(imdb_data$gross/1000000,0))
        dir_rev <- unique(dir_rev[order(-dir_rev[,2]),])
        colnames(dir_rev) <- c("Director", "Gross")
        rownames(dir_rev) <- NULL
        top_20_dir <- dir_rev[1:20,]
        top_20_agg_dir <- aggregate(Gross ~ ., top_20_dir, mean)
        rownames(top_20_agg_dir) <- NULL
        top_20_agg_dir <- unique(top_20_agg_dir[order(-top_20_agg_dir[,2]),])
        rownames(top_20_agg_dir) <- NULL
        return(top_20_agg_dir[1:9,])
      }
      else {
        output$dataMes <- renderText({
          return("Data Frequency")
        })
        if (input$variable == "plot_keywords")
        {
            # Split the words into individual word
            words <- strsplit(as.character(imdb_data[[num]]), split="\\|")
            word_df <- data.frame(unlist(words))
            names(word_df) <- "words"
            catDat <- dplyr::count(word_df, words)
            catDat <- data.frame(catDat[order(-catDat[,2]),])
            return(head(catDat))
        }
        else {
            return(head(as.data.frame(sort(table(imdb_data[[num]]),decreasing = TRUE))))
        }

      }
    }
    else {
      output$dataMes <- renderText({
        return("Summary")
      })
      return(summary(imdb_data[num]))
    }
  })

  
  output$varPlot <- renderPlot({
    num <- fmatch(input$variable, names(imdb_data))
    if (class(imdb_data[[num]]) == "character") {
      if (input$variable %in% c("genres", "plot_keywords"))
      {
        # Split the words into individual word
        words <- strsplit(as.character(imdb_data[[num]]), split="\\|")
        word_df <- data.frame(unlist(words))
        names(word_df) <- "words"
        catDat <- dplyr::count(word_df, words)
        catDat <- catDat[order(-catDat[,2]),]
      }
      else if (input$variable %in% "movie_title"){
          # http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
          # Split the words into individual word
          # docs <- Corpus(VectorSource(imdb_data[[num]]))
          # # Convert the text to lower case
          # docs <- tm_map(docs, content_transformer(tolower))
          # # Remove english common stopwords
          # docs <- tm_map(docs, removeWords, stopwords("english"))
          # # Remove punctuations
          # docs <- tm_map(docs, removePunctuation)
          # # Eliminate extra white spaces
          # docs <- tm_map(docs, stripWhitespace)
          # dtm <- TermDocumentMatrix(docs)
          # m <- as.matrix(dtm)
          # v <- sort(rowSums(m),decreasing=TRUE)
          # catDat <- data.frame(word = names(v),freq=v)
          title_revenue <- data.frame(imdb_data$movie_title, imdb_data$gross/1000000)
          title_rev <- data.frame(title_revenue[order(-title_revenue[,2]),])
          catDat <- title_rev[1:50,]
          rownames(catDat) <- NULL
      }
      else if (input$variable %in% "director_name"){
        
        dir_rev <- data.frame(imdb_data$director_name, round(imdb_data$gross/1000000,0))
        dir_rev <- unique(dir_rev[order(-dir_rev[,2]),])
        colnames(dir_rev) <- c("Director", "Gross")
        rownames(dir_rev) <- NULL
        top_20_dir <- dir_rev[1:20,]
        top_20_agg_dir <- aggregate(Gross ~ ., top_20_dir, mean)
        rownames(top_20_agg_dir) <- NULL
        top_20_agg_dir <- unique(top_20_agg_dir[order(-top_20_agg_dir[,2]),])
        rownames(top_20_agg_dir) <- NULL
        catDat <- data.frame(top_20_agg_dir)
        rownames(catDat) <- NULL
        
      }
      else  {
        catDat <- as.data.frame(sort(table(imdb_data[[num]]),decreasing = TRUE))
      }
      names(catDat) <- c("words", "freq")
      # word cloud
      if (nrow(catDat) >= 50)
      {
        #qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
        #col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
        #color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
        set.seed(200)
        #cols <- colorRampPalette(brewer.pal(12,"Paired"))(nrow(unique(catDat)))
        wordcloud::wordcloud(catDat[1:50,]$words, catDat[1:50,]$freq, colors = sample(brewer.pal(12,"Paired"), 50, replace=T), scale=c(1.7,0.2))
      }
      else {
        set.seed(101)
        wordcloud::wordcloud(catDat$words, catDat$freq, colors = sample(brewer.pal(12,"Paired"), 50, replace=T),scale=c(2.8,0.5))
      }

    }
    else {
      if (input$variable %in% "gross") {
        return(ggplot(imdb_data, aes(x = imdb_data$title_year)) + 
                 geom_line(aes(y = imdb_data$gross), colour="#DD8888") + 
                 ylab(label="Gross") + 
                 xlab("Year") + ggtitle("Gross vs year")+theme(plot.background = element_blank(),
                                                                            panel.background = element_blank(),
                                                                            panel.border = element_blank(),
                                                                            plot.title = element_text(color = "#000000"),
                                                                            legend.position = "none"))
      } else {
        return(ggplot(imdb_data, aes(imdb_data[[num]])) + geom_histogram(aes(y = ..density..), color="black", fill="#DD8888") + xlab(input$variable) +theme(plot.background = element_blank(),
                                                                                                                                                         panel.background = element_blank(),
                                                                                                                                                         panel.border = element_blank(),
                                                                                                                                                         plot.title = element_text(color = "#000000"),
                                                                                                                                                         legend.position = "none"))
      }
    }

  })
  
  # ========= CO-STARRING NETWORK ============
  
  
  # degrees
  output$degree_value <- renderPrint({
    minDegree <- min(g_nodes$degree, na.rm = TRUE)
    maxDegree <- max(g_nodes$degree)
    degreeRange <- c(minDegree, maxDegree)
    return(input$degrees)})
  
   
    ratings <- reactive({
      return(input$ratings)
    })
    
    years <- reactive({
      return(input$years)
    })
    
    movie_years <- reactive({
      return(input$movie_years)
    })
    
    genre <- reactive({
      return(input$genre)
    })
    
    movie_genres <- reactive({
      return(input$movie_genres)
    })
    
    gross <- reactive({
        return(input$gross)
    })
    
    output$rnet <- renderForceNetwork({
      library(igraph)
      library(networkD3)
      nodes_links <- getNL(ratings(), years(), genre(), gross())
      network_nodes <- nodes_links$nodes
      network_links <- nodes_links$links
      
      # Generate degrees
      graph <- graph_from_data_frame(network_links, directed = FALSE, vertices = network_nodes)
      network_nodes$degree <- igraph::degree(graph, mode="all")
      network_nodes$closeness <- igraph::closeness(graph, mode = "all")
      network_nodes$coreness = graph.coreness(as.undirected(graph))
      network_nodes$betweenness <- rescale(igraph::betweenness(graph, normalized = TRUE), to=c(0,200))
      
      # Select up to 100 nodes based on degrees
      #maxDeg <- max(network_nodes$degree)
      #deg <- c(as.integer(maxDeg*0.2), maxDeg)
      network_nodes <- network_nodes[ order(-network_nodes[,3]), ]
      if (nrow(network_nodes) > 100) {
          network_nodes <- network_nodes[1:100,]
      }
      #network_nodes <- subset(network_nodes, network_nodes$degree >= deg[1] & network_nodes$degree <= deg[2])
      network_links <- network_links[network_links$from %in% network_nodes$id,]
      network_links <- network_links[network_links$to %in% network_nodes$id,]
      row.names(network_links) <- NULL
      row.names(network_nodes) <- NULL
      # Reset index for nodes and links
      idn <- as.numeric(rownames((network_nodes))) - 1
      network_nodes <- cbind(idn, network_nodes)
      from2 <- data.frame(from=network_nodes[match(network_links$from, network_nodes$id), 1])
      to2 <- data.frame(to=network_nodes[match(network_links$to, network_nodes$id), 1])
      network_links <- cbind(from2, to2, network_links$value)
      network_nodes <- within(network_nodes, rm(id))
      colnames(network_nodes)[1] <- 'id'
      colnames(network_links)[3] <- 'value'
      # recalculate centrality
      graph2 <- graph_from_data_frame(network_links, directed = FALSE, vertices = network_nodes)
      graph2 <- simplify(graph2)
      network_nodes$degree <- igraph::degree(graph2, mode="all")
      network_nodes$closeness <- igraph::closeness(graph2, mode = "all")
      network_nodes$coreness = graph.coreness(as.undirected(graph2))
      network_nodes$betweenness <- rescale(igraph::betweenness(graph2, normalized = TRUE), to=c(0,100))
      
      # Community
      network_nodes$edgebetweenness <- as.integer(edge.betweenness.community(graph2)$membership)
      network_nodes$walktrap <- as.integer(walktrap.community(graph2, steps=10,modularity=TRUE)$membership)
      network_nodes$fastgreedy <- as.integer(cluster_fast_greedy(graph2)$membership)
      
      #Export network to csv
      
      
      output$i_actors <- renderText({
          influencers <- network_nodes[ order(-network_nodes[,which(colnames(network_nodes) ==input$centrality)]), ]
          paste0(as.character(influencers[1:5,]$name), collapse="\n")
          #head(influencers$name)
      })
      
      observe({
          influencers <- network_nodes[ order(-network_nodes[,which(colnames(network_nodes) == input$centrality)]), ]
          updateSelectInput(session, "td_actor", choices = as.list(influencers[1:5,]$name))
          #input$topActors
      })
      
      # Force network
      forceNetwork(Links = network_links, Nodes = network_nodes, Source = "from", Target = "to", Value = "value",  
                   NodeID = "name", Group = input$community, linkWidth =   JS("function(d) { return d.value; }"), 
                   charge=-20,zoom=T,  
                   Nodesize = input$centrality, fontSize=20, opacity = 0.8, width = NULL, height = NULL)
      
    })
    
    

#================= EGO NETWORK ANALYSIS ====================
    observeEvent(input$switchtab, {
        newtab <- switch(input$tabs, "costar" = "temporal", "temporal" = "costar")
        updateTabItems(session, "tabs", newtab)
    })
    
    # Get input name
    td_actor <- eventReactive(input$search,{
        if (input$td_actor %in% actorList[,1]) {
          output$dmHead <- renderText("")
          output$dm <- renderText("")
            return(input$td_actor)
        }
        else {
            return("")
        }
    })
    
    # get input data
    tda_data <- eventReactive(input$search,{
        if (td_actor() %in% actorList[,1]) {
             getActor(td_actor())
        }
        else {
            return(NULL)
        }
    })
    
    
    output$boPlot <- renderPlot({
      if (!is.null(tda_data())) {
        td_actors <- data.frame(tda_data()$td_data.movie_title, tda_data()$td_data.actor_1_name, tda_data()$td_data.actor_2_name, tda_data()$td_data.actor_3_name)
        colnames(td_actors)[1] <- "actor"
        # co-actors
        td_co_actors <- melt(td_actors, id.vars = "actor")
        names(td_co_actors) <- c("movie_title", "a_num", "actor")
        # add years
        td_years <- data.frame(year=tda_data()[match(td_co_actors$movie_title, tda_data()$td_data.movie_title), 7])
        tda_actor <- cbind(td_co_actors[,3], td_years)
        # add gross
        m_y <- cbind(td_co_actors[,1], td_years)
        td_gross <- data.frame(gross=tda_data()[match(td_co_actors$movie_title, tda_data()$td_data.movie_title), 9])
        gross_year <- cbind(m_y[,2], round(td_gross/1000000, 0))
        gross_year$gross <- as.integer(gross_year$gross)
        colnames(gross_year) <- c("year", "gross")
        g <- ggplot(gross_year, aes(x = year, y = gross, fill=year)) + 
          geom_line() +
          geom_point() +
          theme_classic() + scale_colour_gradientn(colours=rainbow(4)) 
        g
      
      }  
    })
    
    output$tdnet <- renderPlot({
      if (!is.null(tda_data())) {
        td_actors <- data.frame(tda_data()$td_data.movie_title, tda_data()$td_data.actor_1_name, tda_data()$td_data.actor_2_name, tda_data()$td_data.actor_3_name)
        colnames(td_actors)[1] <- "actor"
        # co-actors
        td_co_actors <- melt(td_actors, id.vars = "actor")
        names(td_co_actors) <- c("movie_title", "a_num", "actor")
        # add years
        td_years <- data.frame(year=tda_data()[match(td_co_actors$movie_title, tda_data()$td_data.movie_title), 7])
        tda_actor <- cbind(td_co_actors[,3], td_years)
        
        tda_actor <- aggregate(year ~ ., tda_actor, toString)
        #tda_actor <- as.data.frame(td_co_actors[,3], stringsAsFactors = FALSE)
        #tda_actor <- unique(tda_actor)
        tda_actor <- tda_actor[ order(tda_actor[,1]), ]
        actors_id <- 0:(nrow(tda_actor)-1)
        # nodes
        tda_nodes <- data.frame(actors_id, tda_actor)
        colnames(tda_nodes) <- c("id", "name", "years")
        tda_nodes$name <- as.character(tda_nodes$name)
        rownames(tda_nodes) <- NULL
        # Links
        td_links <- td_co_actors[,c(1,3)]
        names(td_links) <- c("movie_title", "name")
        # change name column 2 to id as from actors_nodes
        td_links_id <- data.frame(movie_title=td_links$movie_title, id= tda_nodes[match(td_links$name, tda_nodes$name), 1])
        td_links_merge <- merge(x=td_links_id, y=td_links_id, by="movie_title")
        td_links_merge <- subset(td_links_merge, td_links_merge$id.x != td_links_merge$id.y)
        # add years  
        td_years <- data.frame(year=tda_data()[match(td_links_merge$movie_title, tda_data()$td_data.movie_title), 7])
        tda_links <- as.data.frame(c(td_links_merge[,c(2,3)], td_years), stringsAsFactors = FALSE)
        # sort links
        tda_links <- tda_links[ order(tda_links[,1], tda_links[,2]), ]
        #names(tda_links) <- c("tail", "head", "onset", "terminus")
        names(tda_links) <- c("from", "to", "year")
        # Add weight to data frame
        #value <- count(tda_links, c("from", "to"))
        
        rownames(tda_links) <- NULL
        # Graph
        tda_graph <- graph_from_data_frame(tda_links, directed = FALSE, vertices = tda_nodes)
        tda_nodes$degree <- igraph::degree(tda_graph, mode="all")
        plot.igraph(tda_graph, 
                    vertex.label = V(tda_graph)$name, vertex.label.color = "gray20",
                    vertex.size = tda_nodes$degree, vertex.size2 = 30,
                    vertex.color = "gray90", vertex.frame.color = "gray20",
                    vertex.shape = "rectangle",
                    edge.arrow.size=0.5, #edge.color=red, #edge.width = E(g)$weight / 10,
                    edge.curved = T, 
                    layout = layout_as_star)
        
       
        
      }
    })
    
    output$csnet <- renderForceNetwork({
      if (!is.null(tda_data())) {
        td_actors <- data.frame(tda_data()$td_data.movie_title, tda_data()$td_data.actor_1_name, tda_data()$td_data.actor_2_name, tda_data()$td_data.actor_3_name)
        colnames(td_actors)[1] <- "movie"
        # co-actors
        td_co_actors <- unique(melt(td_actors, id.vars = "movie"))
        names(td_co_actors) <- c("movie_title", "a_num", "actor")
        # add years
        #td_years <- data.frame(year=tda_data()[match(td_co_actors$movie_title, tda_data()$td_data.movie_title), 7])
        tda_actor <- cbind(td_co_actors[,c(1,3)])
        tda_actor <- aggregate(movie_title ~ ., tda_actor, toString)
        #tda_actor <- as.data.frame(td_co_actors[,3], stringsAsFactors = FALSE)
        #tda_actor <- unique(tda_actor)
        tda_actor <- tda_actor[ order(tda_actor[,1]), ]
        actors_id <- 0:(nrow(tda_actor)-1)
        # nodes
        tda_nodes <- data.frame(actors_id, tda_actor)
        colnames(tda_nodes) <- c("id", "name", "movies")
        tda_nodes$name <- as.character(tda_nodes$name)
        rownames(tda_nodes) <- NULL

        td_links <- td_co_actors[,c(1,3)]
        names(td_links) <- c("movie_title", "name")
        # change name column 2 to id as from actors_nodes
        td_links_id <- data.frame(movie_title=td_links$movie_title, id= tda_nodes[match(td_links$name, tda_nodes$name), 1])
        td_links_merge <- merge(x=td_links_id, y=td_links_id, by="movie_title")
        td_links_merge <- subset(td_links_merge, td_links_merge$id.x != td_links_merge$id.y)
        td_years <- data.frame(year=tda_data()[match(td_links_merge$movie_title, tda_data()$td_data.movie_title), 7])
        tda_links <- as.data.frame(c(td_links_merge[,c(2,3,1)], td_years), stringsAsFactors = FALSE)
        tda_links <- tda_links[ order(tda_links[,1], tda_links[,2]), ]
        
        # sort links
        tda_links <- as.data.frame(c(td_links_merge[,c(2,3)]), stringsAsFactors = FALSE)
        tda_links <- tda_links[ order(tda_links[,1], tda_links[,2]), ]
        tda_links <- ddply(tda_links,.(tda_links[,1],tda_links[,2]),nrow)
        names(tda_links) <- c("from", "to", "value")
        rownames(tda_links) <- NULL

        # Generate degrees
        tda_graph <- graph_from_data_frame(tda_links, directed = FALSE, vertices = tda_nodes)
        tda_graph <- simplify(tda_graph)
        tda_nodes$degree <- igraph::degree(tda_graph, mode="all")
        tda_nodes$betweenness <- rescale(igraph::betweenness(tda_graph, normalized = TRUE), to=c(0,200))
        # Community
        tda_nodes$walktrap <- as.integer(walktrap.community(tda_graph, steps=5,modularity=TRUE)$membership)
        tda_nodes$group <- as.integer(cluster_leading_eigen(tda_graph)$membership)
        #tda_nodes$group <- as.integer(cluster_label_prop(tda_graph)$membership) 
        
        forceNetwork(Links = tda_links, Nodes = tda_nodes, Source = "from", Target = "to", Value = "value",
                     NodeID = "name", Group = "group", linkWidth =   JS("function(d) { return d.value; }"),
                     charge=-80,zoom=T, clickAction = 'Shiny.onInputChange("id", d.name)',
                     Nodesize = "betweenness", fontSize=12, opacity = 0.9, width = NULL, height = NULL)

      }
    })
    
    observeEvent(input$id, {
      mov <- unique(tda_data()[apply(tda_data() [c('td_data.actor_1_name','td_data.actor_2_name','td_data.actor_3_name')],1,function(x) any(x %in% input$id)), c(1,7)])
      colnames(mov) <- c("Movie", "Year")
      rownames(mov) <- NULL
      jmov <- toJSON(list(mov),pretty = TRUE, auto_unbox = TRUE)
      #session$sendCustomMessage(type = 'testmovies',message = list(id=input$id, l=mov))
      output$dmHead <- renderText({
        paste0(td_actor(), " and ", input$id, " starred in the following movie(s):")
      })
      #HTML(paste0(td_actor(), " and ", input$id, " starred in the following movie(s):"))
      output$dm <- renderTable({
        mov <- unique(tda_data()[apply(tda_data() [c('td_data.actor_1_name','td_data.actor_2_name','td_data.actor_3_name')],1,function(x) any(x %in% input$id)), c(1,7)])
        colnames(mov) <- NULL
        rownames(mov) <- NULL
        #paste0(td_actor(), " and ", input$id, " starred in the following movie(s):")
        mov
      }, spacing="xs")
    })
    
    
    output$ggPlots <- renderPlot({
      if (!is.null(tda_data())) {
        td_actors <- data.frame(tda_data()$td_data.movie_title, tda_data()$td_data.actor_1_name, tda_data()$td_data.actor_2_name, tda_data()$td_data.actor_3_name)
        colnames(td_actors)[1] <- "movie"
        # co-actors
        td_co_actors <- melt(td_actors, id.vars = "movie")
        names(td_co_actors) <- c("movie_title", "a_num", "actor")
        # add years
        td_years <- data.frame(year=tda_data()[match(td_co_actors$movie_title, tda_data()$td_data.movie_title), 7])
        tda_actor <- cbind(td_co_actors[,3], td_years)
        tda_actor <- aggregate(year ~ ., tda_actor, toString)
        #tda_actor <- as.data.frame(td_co_actors[,3], stringsAsFactors = FALSE)
        #tda_actor <- unique(tda_actor)
        tda_actor <- tda_actor[ order(tda_actor[,1]), ]
        actors_id <- 0:(nrow(tda_actor)-1)
        # nodes
        tda_nodes <- data.frame(actors_id, tda_actor)
        colnames(tda_nodes) <- c("id", "name", "years")
        tda_nodes$name <- as.character(tda_nodes$name)
        rownames(tda_nodes) <- NULL
        
        td_links <- td_co_actors[,c(1,3)]
        names(td_links) <- c("movie_title", "name")
        # change name column 2 to id as from actors_nodes
        td_links_id <- data.frame(movie_title=td_links$movie_title, id= tda_nodes[match(td_links$name, tda_nodes$name), 1])
        td_links_merge <- merge(x=td_links_id, y=td_links_id, by="movie_title")
        td_links_merge <- subset(td_links_merge, td_links_merge$id.x != td_links_merge$id.y)
        # add range of years
        td_years <- data.frame(year=tda_data()[match(td_links_merge$movie_title, tda_data()$td_data.movie_title), 7])
        td_years$bin <- ifelse(td_years$year < 1990, "1927-1990",
                        ifelse(td_years$year >= 1990 & td_years$year < 2000, "1990-1999",
                        ifelse(td_years$year >= 2000 & td_years$year < 2010, "2000-2009",
                        ifelse(td_years$year >= 2010, "2010-2016",
                        NA  ))))
        tda_links <- as.data.frame(c(td_links_merge[,c(2,3)], td_years), stringsAsFactors = FALSE)
        # sort links
        tda_links <- tda_links[ order(tda_links[,1], tda_links[,2]), ]
        names(tda_links) <- c("from", "to", "year", "range")
        rownames(tda_links) <- NULL
        
        graph <- graph_from_data_frame(d=tda_links, vertices=tda_nodes, directed=T)
        V(graph)$degree <- igraph::degree(graph, mode = 'all')
        V(graph)$col <- V(graph)$degree
        E(graph)$range <- as.character(E(graph)$range)
        
        p <- ggraph(graph, layout="kk") + 
          geom_edge_link(aes(colour=factor(range))) + 
          geom_node_point(aes(size = degree)) + #scale_edge_alpha(guide="None") +
          theme_graph() 
        p + facet_edges( ~range, ncol = 2) + 
          theme_graph(foreground = 'steelblue', fg_text_colour = 'white', strip_text_size = 16, base_size = 14)
         
      }
    })
    
    
    output$dTable <- renderDataTable({
      if (!is.null(tda_data())) {
        dTab <- data.frame(tda_data()$td_data.title_year, tda_data()$td_data.movie_title, tda_data()$td_data.actor_1_name, tda_data()$td_data.actor_2_name, tda_data()$td_data.actor_3_name)
        colnames(dTab) <- c("Year", "Title", "Actor1", "Actor2", "Actor3") 
        dTab <- unique(dTab[order(dTab[,1]),])
        rownames(dTab) <- NULL
        dTab
      }
    },options = list(
      autoWidth = TRUE, scrollX=TRUE, pageLength=6,
      columnDefs = list(list(width = '5px', targets = 1))
      #columnDefs = list(list(width = '100px', targets = "_all"))
    ))
    
    output$tdPlots <- renderPlot({
      if (!is.null(tda_data())) {
        td_actors <- data.frame(tda_data()$td_data.movie_title, tda_data()$td_data.actor_1_name, tda_data()$td_data.actor_2_name, tda_data()$td_data.actor_3_name)
        colnames(td_actors)[1] <- "actor"
        # co-actors
        td_co_actors <- melt(td_actors, id.vars = "actor")
        names(td_co_actors) <- c("movie_title", "a_num", "actor")
        # add years
        td_years <- data.frame(year=tda_data()[match(td_co_actors$movie_title, tda_data()$td_data.movie_title), 7])
        tda_actor <- cbind(td_co_actors[,3], td_years)
        tda_actor <- aggregate(year ~ ., tda_actor, toString)
        #tda_actor <- as.data.frame(td_co_actors[,3], stringsAsFactors = FALSE)
        #tda_actor <- unique(tda_actor)
        tda_actor <- tda_actor[ order(tda_actor[,1]), ]
        actors_id <- 1:(nrow(tda_actor))
        # nodes
        tda_nodes <- data.frame(actors_id, tda_actor)
        colnames(tda_nodes) <- c("id", "name", "years")
        tda_nodes$name <- as.character(tda_nodes$name)
        rownames(tda_nodes) <- NULL
        
        td_links <- td_co_actors[,c(1,3)]
        names(td_links) <- c("movie_title", "name")
        # change name column 2 to id as from actors_nodes
        td_links_id <- data.frame(movie_title=td_links$movie_title, id= tda_nodes[match(td_links$name, tda_nodes$name), 1])
        td_links_merge <- merge(x=td_links_id, y=td_links_id, by="movie_title")
        td_links_merge <- subset(td_links_merge, td_links_merge$id.x != td_links_merge$id.y)
        # add years
        td_years <- data.frame(year=tda_data()[match(td_links_merge$movie_title, tda_data()$td_data.movie_title), 7])
        tda_links <- as.data.frame(c(td_links_merge[,c(2,3)], td_years), stringsAsFactors = FALSE)
        # sort links
        tda_links <- tda_links[ order(tda_links[,1], tda_links[,2]), ]
        names(tda_links) <- c("from", "to", "year")
        rownames(tda_links) <- NULL
        
        tda_links$timebins <- as.numeric(cut(tda_links$year, breaks=length(unique(tda_links$year))))
        # Fading
        edgesAnim <- lapply(1:10, function(i) {
          tda_links$timebins <- tda_links$timebins + i;
          tda_links$delay <- i;
          tda_links
        })
        tda_links$delay <- 0
        edgesAnim <- rbind(tda_links, do.call(rbind, edgesAnim))
        edgesGraph <- graph_from_data_frame(edgesAnim, directed=F)
        # Original Layout
        SubGr <- subgraph.edges(edgesGraph, which(E(edgesGraph)$delay == 0))
        V(SubGr)$degree <- igraph::degree(SubGr)
        V(SubGr)$group <- igraph::cluster_edge_betweenness(subGr)$membership
        lay <- create_layout(SubGr, 'igraph', algorithm="lgl")
        
        attr(lay, 'graph') <- edgesGraph
        
        # Create graph with timebins as frame
        p <- ggraph(data=lay) +
          geom_node_point(aes(size=degree, colour = factor(group))) +
          geom_edge_link0(aes(frame=timebins, alpha=delay, width=delay, colour=factor(node1.group)), data = gEdges(nodePar="group")) +
          scale_edge_alpha(range=c(0,1), guide = "none") +
          scale_edge_width(range=c(0.5,1.5), trans="exp", guide = "none") +
          scale_size(guide = "none") +
          ggforce::theme_no_axes() +
          theme(plot.background = element_rect(fill="#000000"),
                panel.background = element_blank(),
                panel.border = element_blank(),
                plot.title = element_text(color = "#000000"),
                legend.position = "none")
       p 
       
       # animation
       
       animation::ani.options(interval=0.2)
       gganimation(p, "animation.mp4", title_frame=FALSE)
       
        # # Setup network
        # net <- network(tda_links,  vertex.attr=tda_nodes, matrix.type="edgelist", 
        #                loops=F, multiple=F, ignore.eval = F)
        # # Setup initial layout
        # x <- gplot.layout.fruchtermanreingold(net, NULL)
        # net %v% "x" <- x[,1]
        # net %v% "y" <- x[,2]
        # 
        # guide <- guides(color = FALSE, size = FALSE)
        # # A temp file to save the output.
        # # This file will be removed later by renderImage
        # #outfile <- tempfile(fileext = '.gif')
        # ani.record(reset = TRUE)
        # # Generate the PNG
        # #png(outfile, width = 600, height = 400)
        # saveGIF(
        #       for (t in c(min(tda_links$year):max(tda_links$year))){
        #         t_value <- c()
        #         for (i in tda_nodes$years) {
        #           c <- unlist(strsplit(i, ","))
        #           if (any(trimws(c) %in% t)) {
        #             t_value <- c(t_value, 1)
        #           }
        #           else {
        #             t_value <- c(t5_value, 2)
        #           }
        #         }
        #         net %v% 't' <- t_value 
        #         net %v% "color" = ifelse(net %v% "t" == "1", "purple", "lightgray")
        #         g = ggnet2(net, mode=c("x","y"), size = "degree", label=T, label.size = 4, label.color="white", edge.color = c("color", "grey50"), color = "purple") + guide
        #         #p <- ggplot(net, aes(x = from, y = to, xend = to-1, yend = from+1, frame= tda_links$year))
        #         #saveGIF(g,filename=paste0("output_",t,".png", sep=""))
        #         #ani.record()
        #       }, movie.name = 'random.gif', interval = 0.1)
        #     file.rename('random.gif', file)
        #  
        # #saveGIF(ani.replay(), img.name = "record_plot")
        # #p <- ggplot(tda_links, aes(x = from, y = to, xend = from, yend = to, frame=year))
        # 
        # #gganimate(p,"outfile.gif", title_frame =FALSE)
        # 
        # #dev.off()
        # 
        # # Return a list containing the filename
        # list(src = "outfile.gif",
        #      contentType = 'image/gif',
        #      width = 600,
        #      height = 400,
        #      alt = "Temporal Dynamic Analysis")
          }
    })#, deleteFile = TRUE)
    
    # output$ndtvPlots <- renderPlot({
    #   if (!is.null(tda_data())) {
    #     
    #     td_actors <- data.frame(tda_data()$td_data.movie_title, tda_data()$td_data.actor_1_name, tda_data()$td_data.actor_2_name, tda_data()$td_data.actor_3_name)
    #     colnames(td_actors)[1] <- "actor"
    #     # co-actors
    #     td_co_actors <- melt(td_actors, id.vars = "actor")
    #     names(td_co_actors) <- c("movie_title", "a_num", "actor")
    #     # add years
    #     td_years <- data.frame(year=tda_data()[match(td_co_actors$movie_title, tda_data()$td_data.movie_title), 7])
    #     tda_actor <- cbind(td_co_actors[,3], td_years)
    #     tda_actor <- aggregate(year ~ ., tda_actor, toString)
    #     tda_actor <- tda_actor[ order(tda_actor[,1]), ]
    #     actors_id <- 1:(nrow(tda_actor))
    #     # nodes
    #     tda_nodes <- data.frame(actors_id, tda_actor)
    #     colnames(tda_nodes) <- c("id", "name", "years")
    #     tda_nodes$name <- as.character(tda_nodes$name)
    #     rownames(tda_nodes) <- NULL
    #     
    #     td_links <- td_co_actors[,c(1,3)]
    #     names(td_links) <- c("movie_title", "name")
    #     # change name column 2 to id as from actors_nodes
    #     td_links_id <- data.frame(movie_title=td_links$movie_title, id= tda_nodes[match(td_links$name, tda_nodes$name), 1])
    #     td_links_merge <- merge(x=td_links_id, y=td_links_id, by="movie_title")
    #     td_links_merge <- subset(td_links_merge, td_links_merge$id.x != td_links_merge$id.y)
    #     # add years
    #     td_years <- data.frame(year=tda_data()[match(td_links_merge$movie_title, tda_data()$td_data.movie_title), 7])
    #     tda_links <- as.data.frame(c(td_links_merge[,c(2,3)], td_years, td_years), stringsAsFactors = FALSE)
    #     # sort links
    #     tda_links <- tda_links[ order(tda_links[,1], tda_links[,2]), ]
    #     names(tda_links) <- c("tail", "head", "onset", "terminus")
    #     rownames(tda_links) <- NULL
    #     
    #     # Dynamic network
    #     node_onset <- as.data.frame(do.call(rbind, lapply(strsplit(as.character(tda_nodes$years), split=","), function(y) min(as.numeric(y)))))
    #     node_terminus <- as.data.frame(do.call(rbind, lapply(strsplit(as.character(tda_nodes$years), split=","), function(y) max(as.numeric(y)))))
    #     vs <- data.frame(onset=node_onset, terminus=node_terminus+1, vertex.id=tda_nodes[,1], name = tda_nodes[,2])
    #     colnames(vs) <- c("onset", "terminus", "id", "name")
    #     vs$name <- as.character(vs$name)
    #     es <- data.frame(onset=tda_links[,3], terminus=tda_links[,3]+1, 
    #                      tail=tda_links[,1], head=tda_links[,2])
    #     
    #     
    #     td_net3 <- networkDynamic(vertex.spells=vs,
    #                               edge.spells=es)
    #     td_net3 %v% 'name' <- vs$name
    #     # make it discrete 
    #     colfunc <- colorRampPalette(brewer.pal(7,"Accent"))
    #     x <- gplot.layout.target(td_net3, NULL)
    #     td_net3 %v% "x" <- x[,1]
    #     td_net3 %v% "y" <- x[,2]
    #     
    #     
    #     compute.animation(td_net3, animation.mode = "kamadakawai", layout.par = c("x", "y"),
    #                       slice.par=list(start=min(es$onset), end=max(es$terminus), interval=1,
    #                                      aggregate.dur=1, rule='any'))
    # 
    #     colfunc <- colorRampPalette(brewer.pal(7,"Accent"))
    #     render.d3movie(td_net3, usearrows = F, displaylabels= T,
    #                    bg='#c0c0c0', vertex.border="#dddddd", edge.col = '#fd9696',
    #                    vertex.cex = 3,
    #                    vertex.col = colfunc(nrow(es)),
    #                    vertex.col = "#000000",
    #                    edge.lwd = 2,
    #                    output.mode='htmlWidget',
    #                    script.type='embedded', 
    #                    launchBrowser=TRUE)
    #     
    #   }})
    
    output$tdHeader <- renderText({
      if (!is.null(tda_data())) {
        paste0(td_actor() ," network")
      }
    })
    
    output$actHeader <- renderText({
      if (!is.null(tda_data())) {
        paste0("Number of ",td_actor() ,"'s movies vs years ")
      }
    })
    
    output$ggHeader <- renderText({
      if (!is.null(tda_data())) {
        paste0(td_actor() ," time slice visualisation")
      }
    })
    
    output$gtHeader <- renderText({
      if (!is.null(tda_data())) {
        paste0(td_actor() ," genres trend analysis with streamgraph")
      }
    })
    
    output$barHeader <- renderText({
      if (!is.null(tda_data())) {
        paste0(td_actor() ," genres trend analysis with bar plot")
      }
    })
    
    output$sbHeader <- renderText({
      if (!is.null(tda_data())) {
        paste0(td_actor() ," collaboration with directors, movies and years")
      }
    })
    
    # Sunburst of actor, movie and director
    output$sunburst <- renderSunburst({
        if (!is.null(tda_data())) {
              amd_actor <- gsub('-', "_", td_actor())
              amd_movie <- gsub('-', "_", tda_data()$td_data.movie_title)
              amd_director <- gsub('-', "_", tda_data()$td_data.director_name)
              amd_year <- gsub('-', "_", tda_data()$td_data.title_year)
              amd_gross <- gsub('-', "_", round(tda_data()$td_data.gross/1000000),0)
              amd_paste <- paste(amd_actor,amd_director, amd_movie, amd_year,sep='-')
              #sb_amd <- unique(data.frame(amd_paste, rep(1, length(amd_paste))))
              sb_amd <- unique(data.frame(amd_paste, amd_gross))
              sb_amd <- sb_amd[order(sb_amd[,2]),]
              add_shiny(sunburst(sb_amd, explanation="function(d){return d.data.amd_actor}", percent = FALSE))
        }
    })
    selection <- reactive({
        input$sunburst_mouseover
    })
    output$selection <- renderText(selection())
    
    
    output$activeYear <- renderPlot({
      
      if (!is.null(tda_data())) {
        title_year <- data.frame(tda_data()$td_data.title_year, tda_data()$td_data.movie_title, tda_data()$td_data.genres) 
        title_year <- title_year[order(title_year[,1]),]
        colnames(title_year) <- c("year", "title", "genres")
        ty <- data.table(title_year)
        ty[ , count := 1:.N , by = "year" ]
        genres_years <- ty[,c(1,3,4)] 
        
        genres_split <- strsplit(as.character(genres_years$genres), split="\\|")
        genres_years_split <- data.frame(year = rep(genres_years$year, sapply(genres_split, length)), count = rep(1/sapply(genres_split, length), sapply(genres_split, length)),genre = unlist(genres_split), stringsAsFactors = FALSE)
        genres_years_split <- genres_years_split[order(genres_years_split[,1], genres_years_split[,3]),]
        cols <- colorRampPalette(brewer.pal(12, "Paired"))(length(unique(genres_years_split$genre)))
        # The palette with grey:
        g <- ggplot(genres_years_split, aes(x = year, y = count, group = year, fill = genre)) + 
          geom_bar(stat="identity") + theme_classic() + scale_fill_manual(values=cols) + ylab("Number of movies") 
          
        g
      }
    })
    
    rv <- reactiveValues(func=NULL) 
    
    
    
    Gyear <- reactive({
      if (!is.null(tda_data())) {
        minY <- min(tda_data()$td_data.title_year)
        maxY <- max(tda_data()$td_data.title_year)
        val1 <- max(minY, input$Gyears[1])
        val2 <- min(maxY, input$Gyears[2])
        valueY <- c(val1, val2)
        updateSliderInput(session, "Gyears", min = minY, max = maxY, value=valueY)
        
        input$Gyears
      }  
    })
    
    observeEvent(input$search,{ 
      if (!is.null(tda_data())) {
        minY <- min(tda_data()$td_data.title_year)
        maxY <- max(tda_data()$td_data.title_year)
        updateSliderInput(session, "Gyears", min = minY, max = maxY, value=c(minY, maxY))
        
      }  
      
      
    })
    
    
    output$lgnet <- renderPlot({
      genres_data <- subset(imdb_data, imdb_data$title_year >= movie_years()[1] &
                              imdb_data$title_year <= movie_years()[2])
      if (!str_detect(movie_genres(), "All")) {
        genres_data <- genres_data[grep(movie_genres(), genres_data$genres),]}
      genres_title <- data.frame(genres_data$movie_title, genres_data$genres) 
      colnames(genres_title) <- c("titler", "genres")
      genres_split <- strsplit(as.character(genres_title$genres), split="\\|")
      genres_title_split <- data.frame(title = rep(genres_title$title, sapply(genres_split, length)), genre = unlist(genres_split), stringsAsFactors = FALSE)
      # Add gross to a column
      genre_gross <- data.frame(gross=genres_data[match(genres_title_split $title, genres_data$movie_title), 9])
      gtg <- as.data.frame(c(genres_title_split[,c(1,2)], genre_gross), stringsAsFactors = FALSE)
      gtg <- gtg[ order(-gtg[,3]), ]
      rownames(gtg) <- NULL
      # Get the last occurence index of the movies
      i <- which(!duplicated(gtg[,1],fromLast=T))
      # Filter the top 20 movies
      top20_gtg <- gtg[c(1:i[20]),]
      # add gross to two_mode_nodes
      gr <- top20_gtg[,2:3]
      gr$gross <- as.numeric(gr$gross)
      gross_genres <- aggregate(gross ~ ., gr, sum)
      
      # Generate nodes and links
      two_mode_nodes <- unique(data.frame(id = c(as.matrix(top20_gtg[,1:2]))))
      two_mode_nodes$id <- as.character(two_mode_nodes$id)
      genreList <- unique(genres_title_split$genre)
      two_mode_nodes$type <- ifelse(two_mode_nodes[,1] %in%  genreList, 1, NA)
      two_mode_nodes <- two_mode_nodes[ order(-two_mode_nodes[,2], two_mode_nodes[,1]),]
      two_mode_nodes$gross <- c(gross_genres[,2], rep(NA, nrow(two_mode_nodes)-nrow(gross_genres)))
      rownames(two_mode_nodes) <- NULL
      # Generate links
      tm_links <- top20_gtg[,c(2,1)]
      #gglinks
      gg_links <- cbind(tm_links, type=rep("genres", nrow(tm_links)))
      m <- as.matrix(table(tm_links))
      M <- m[, colnames(m) %in% unique(top20_gtg[,1])]
      two_mode_links <- rbind(M)
      net2 <- graph_from_incidence_matrix(two_mode_links)
      table(V(net2)$type)
      #plot(net2, vertex.label=NA)
      # Genres are blue squares, movie nodes are orange circles:
      V(net2)$color <- c("#ccebc5", "#fddaec")[V(net2)$type+1]
      V(net2)$shape <- c("circle", "circle")[V(net2)$type+1]
      # Genres will have name labels, movie nodes  will not:
      V(net2)$label <- ""
      V(net2)$label[V(net2)$type==F] <- two_mode_nodes$id[V(net2)$type==F] 
      V(net2)$label[V(net2)$type==T] <- two_mode_nodes$id[V(net2)$type==T] 
      V(net2)$label.cex=0.6
      V(net2)$label.font=2.5
      V(net2)$size <- rescale(round(two_mode_nodes$gross/1000000,0),c(5, 20))
      #l <- layout_in_circle(net2)
      l <- layout.bipartite(net2)
      
      
      #png("network.png",height=260, width=400)
      #coords <- layout.auto(net2)
      par(mar=c(0,0,0,0))
      plot(net2, vertex.label.color=ifelse(V(net2)$type,"black", "black"), vertex.size=ifelse(V(net2)$type,10, V(net2)$size), vertex.label.degree=pi/2,
           vertex.frame.color="white",layout=l[,c(2,1)]) 
      
    })
      
    
    output$gennet <- renderPlot({
      genres_data <- subset(imdb_data, imdb_data$title_year >= movie_years()[1] &
                              imdb_data$title_year <= movie_years()[2])
      if (!str_detect(movie_genres(), "All")) {
        genres_data <- genres_data[grep(movie_genres(), genres_data$genres),]}
      genres_title <- data.frame(genres_data$movie_title, genres_data$genres) 
      colnames(genres_title) <- c("titler", "genres")
      genres_split <- strsplit(as.character(genres_title$genres), split="\\|")
      genres_title_split <- data.frame(title = rep(genres_title$title, sapply(genres_split, length)), genre = unlist(genres_split), stringsAsFactors = FALSE)
      # Add gross to a column
      genre_gross <- data.frame(gross=genres_data[match(genres_title_split $title, genres_data$movie_title), 9])
      gtg <- as.data.frame(c(genres_title_split[,c(1,2)], genre_gross), stringsAsFactors = FALSE)
      gtg <- gtg[ order(-gtg[,3]), ]
      rownames(gtg) <- NULL
      # Get the last occurence index of the movies
      i <- which(!duplicated(gtg[,1],fromLast=T))
      # Filter the top 20 movies
      top20_gtg <- gtg[c(1:i[20]),]
      # Generate nodes and links
      two_mode_nodes <- unique(data.frame(id = c(as.matrix(top20_gtg[,1:2]))))
      # add gross to two_mode_nodes
      gr <- top20_gtg[,2:3]
      gr$gross <- as.numeric(gr$gross)
      gross_genres <- aggregate(gross ~ ., gr, sum)
      
      two_mode_nodes$id <- as.character(two_mode_nodes$id)
      genreList <- unique(genres_title_split$genre)
      two_mode_nodes$type <- ifelse(two_mode_nodes[,1] %in%  genreList, 1, NA)
      two_mode_nodes <- two_mode_nodes[ order(-two_mode_nodes[,2], two_mode_nodes[,1]),]
      two_mode_nodes$gross <- c(gross_genres[,2], rep(NA, nrow(two_mode_nodes)-nrow(gross_genres)))
      rownames(two_mode_nodes) <- NULL
      # Generate links
      tm_links <- top20_gtg[,c(2,1)]
      #gglinks
      gg_links <- cbind(tm_links, type=rep("genres", nrow(tm_links)))
      m <- as.matrix(table(tm_links))
      M <- m[, colnames(m) %in% unique(top20_gtg[,1])]
      two_mode_links <- rbind(M)
      net2 <- graph_from_incidence_matrix(two_mode_links)
      table(V(net2)$type)
      #plot(net2, vertex.label=NA)
      # Genres are blue squares, movie nodes are orange circles:
      V(net2)$color <- c("#ccebc5", "#fddaec")[V(net2)$type+1]
      V(net2)$shape <- c("circle", "circle")[V(net2)$type+1]
      # Genres will have name labels, movie nodes  will not:
      V(net2)$label <- ""
      V(net2)$label[V(net2)$type==F] <- two_mode_nodes$id[V(net2)$type==F] 
      V(net2)$label[V(net2)$type==T] <- two_mode_nodes$id[V(net2)$type==T] 
      V(net2)$label.cex=0.6
      V(net2)$label.font=2
      V(net2)$name <- two_mode_nodes$id
      V(net2)$size <- rescale(round(two_mode_nodes$gross/1000000,0),c(5, 20))
      l <- layout_as_bipartite(net2)
      
      
      par(mar=c(0,0,0,0))
      plot(net2, vertex.label.color=ifelse(V(net2)$type,"black", "black"), vertex.size=ifelse(V(net2)$type,10, V(net2)$size), vertex.label.degree=pi/2,
           vertex.frame.color="white",layout=l[,c(2,1)])
      
    }, height=200)
    
    
    output$stackPlot <- renderPlot({
      genres_data <- subset(imdb_data, imdb_data$title_year >= movie_years()[1] &
                              imdb_data$title_year <= movie_years()[2])
        if (!str_detect(movie_genres(), "All")) {
          genres_data <- genres_data[grep(movie_genres(), genres_data$genres),]}
        genres_year <- data.frame(genres_data$title_year, genres_data$genres) 
        colnames(genres_year) <- c("year", "genres")
        
        genres_split <- strsplit(as.character(genres_year$genres), split="\\|")
        genres_year_split <- data.frame(year = rep(genres_year$year, sapply(genres_split, length)), genre = unlist(genres_split), stringsAsFactors = FALSE)
        genres_year_split <- cbind(genres_year_split, rep(1, nrow(genres_year_split)))
        colnames(genres_year_split)[3] <- "n"
        genres_year_agg <- aggregate(n ~ genres_year_split$year + genres_year_split$genre, data = genres_year_split, length)
        colnames(genres_year_agg) <- c("year", "genre", "n")
        sorted_genres_year <- na.omit(genres_year_agg[ order(c(genres_year_agg$year, genres_year_agg$genre)), ])
        all_genres_year <- as.data.frame(xtabs(n~year+genre, sorted_genres_year))
        all_genres_year$year <- as.numeric(as.character(all_genres_year$year))
        colnames(all_genres_year) <- c("year", "genre", "n")
        
        ### Output freq words
        freq_genres <- aggregate(all_genres_year$n ~ all_genres_year$genre, all_genres_year, FUN=sum)
        colnames(freq_genres) <- c("genre", "freq")
        output$freqPlot <- renderPlot({
          wordcloud(freq_genres$genre, freq_genres$freq, colors = brewer.pal(8, "Dark2"),scale=c(2.2,0.3))
        })
        # Update slider input years
        colfunc<-colorRampPalette(brewer.pal(9,"Set3"))
        if (!str_detect(movie_genres(), "All")) {
          cols <- colorRampPalette(brewer.pal(12, "Paired"))(length(unique(all_genres_year$genre)))
          selected_genre_year <- all_genres_year[grep(movie_genres(), all_genres_year$genre), ]
          g <- ggplot(selected_genre_year, aes(x = year, y = n, group = genre, fill = genre)) +
            geom_area(aes(fill=genre,group = genre)) + guides(color=FALSE) + ggtitle(movie_genres()) +
            theme_classic() + ylab("No of occurence") + guides(fill=FALSE) + scale_fill_manual(values=cols)
          
        } else {
          cols <- colorRampPalette(brewer.pal(12, "Paired"))(length(unique(all_genres_year$genre)))
          g <- ggplot(all_genres_year, aes(x = year, y = n, group = genre, fill = genre)) +
            stat_steamgraph() + scale_fill_manual(values=cols) + 
            theme_classic() + ylab("No of occurence") + 
            theme(legend.key.height = unit(0.15,"cm")) + 
            theme(legend.key.width = unit(0.2,"cm")) +
            guides(fill=guide_legend(ncol=1))
            
        }
        g
       
      })
    
    
    output$barPlot <- renderPlot({
      if (!is.null(tda_data())) {
        genres_year <- data.frame(tda_data()$td_data.title_year, tda_data()$td_data.genres) 
        colnames(genres_year) <- c("year", "genres")
        
        title_year <- data.frame(tda_data()$td_data.title_year, tda_data()$td_data.movie_title) 
        colnames(title_year) <- c("year", "title")
        agg_title <- aggregate(title ~ year, title_year, length)
        
        genres_split <- strsplit(as.character(genres_year$genres), split="\\|")
        genres_year_split <- data.frame(year = rep(genres_year$year, sapply(genres_split, length)), genre = unlist(genres_split), stringsAsFactors = FALSE)
        genres_year_split <- cbind(genres_year_split, rep(1, nrow(genres_year_split)))
        colnames(genres_year_split)[3] <- "n"
        genres_year_agg <- aggregate(n ~ genres_year_split$year + genres_year_split$genre, data = genres_year_split, length)
        colnames(genres_year_agg) <- c("year", "genre", "n")
        sorted_genres_year <- na.omit(genres_year_agg[ order(c(genres_year_agg$year, genres_year_agg$genre)), ])
        all_genres_year <- as.data.frame(xtabs(n~year+genre, sorted_genres_year))
        all_genres_year$year <- as.numeric(as.character(all_genres_year$year))
        colnames(all_genres_year) <- c("year", "genre", "n")
        # Count number of title
        title_sum <- data.frame(cnt=agg_title$title, year= all_genres_year[match(agg_title$year, all_genres_year$year), 1])
        ty <- data.frame(title = rep(title_sum$cnt, sapply(genres_split, length)), genre = unlist(genres_split), stringsAsFactors = FALSE)
        
        
        #wordcloud(all_genres_year$genre, all_genres_year$n)
        #Update slider input years
        sub_genres_year <- subset(all_genres_year, all_genres_year$year >= Gyear()[1] & all_genres_year$year <= Gyear()[2])
        #colfunc<-colorRampPalette(brewer.pal(9,"Set3"))
        cols <- colorRampPalette(brewer.pal(8, "Accent"))(length(unique(sub_genres_year$genre)))
        g <- ggplot(sub_genres_year, aes(x = year, y = n, group = genre, fill = genre)) +
          #geom_bar(stat="identity") + theme_classic() + scale_fill_manual(values=cols)
          stat_steamgraph() + scale_fill_manual(values=cols) + 
          theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), panel.background = element_blank())  
        g
        
        
      }
      
      
    })
    
})
