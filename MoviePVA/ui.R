# =================================================================
# COMP5703 - IT CAPSTONE PROJECT - SCALABLE MOVIE VISUAL ANALYTICS
# =================================================================
# Author: Marni Torkel
# Codes are based on tutorial from the following:
# Github, Kaggles, R Studio, Kateto.net, Stackoverflow, etc
# ================================================================
#
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(leaflet)
library(networkD3)
library(sunburstR)
library(htmlwidgets)
library(DT)
library(shinysky)
library(network)
library(networkDynamic)
library(lazyeval)
library(edgebundleR)
library(leaflet)
library(ggplot2)
library(ggTimeSeries)
library(ggthemes)

hoverJS <- 'd3.selectAll(".node").on("mouseover", function(d) { 
   d3.select(this).select("text").style("visibility", "visible"); 
})'



# Header
header <- dashboardHeader(
    
  title="Movie Visual Analytics"
  
  ) 

# Sidebar
sidebar <- dashboardSidebar(
      sidebarMenu(id = "tabs",
        menuItem("Overview", tabName = "overview", icon=icon("film")),
        menuItem("Actors", tabName = "costar", icon=icon("users")),
          menuItem("Movies", tabName = "trend", icon=icon("film"))
          
      )
)

# Body
body <- dashboardBody(
  tags$head(
    tags$style(HTML('#search{background-color:black; color:white; margin-top:25px;}'))),
    tags$head(tags$script(HTML("
                         $(document).ready(function(e) {
                               $('.sunburst-sidebar').remove();
                               })
                               "))),    
  #tag$head(tags$script(src = hoverJS)),
  tags$head(tags$style('
     #tooltip {
      position: absolute;
      width: 300px;
      z-index: 100;
     }
  ')),
  tags$script('Shiny.addCustomMessageHandler("testmovies",function(message) {
    for (variable in message.l) {
      alert(variable)
    }
    })'),                    
  tags$script('
    $(document).ready(function(){
      // id of the plot
      $("#stackPlot").mousemove(function(e){ 

        // ID of uiOutput
        $("#my_tooltip").show();         
        $("#my_tooltip").css({             
          top: (e.pageY + 5) + "px",             
          left: (e.pageX + 5) + "px"         
        });     
      });     
    });
  '),
  # ========== OVERVIEW ==========
  tabItems(
    tabItem(tabName = "overview",
            fluidRow(
              column(width=12, 
                     box(title="IMDB Overview", width=NULL, height=170,status= "primary", solidHeader=TRUE, 
                        fluidRow(
                          column(width=2, img(src="dataset-thumbnail.png", align="center", width="110px", height="110px")),
                          column(width=10,"IMDB (Internet Movie Database) is a popular online database of information related to movies from around the world. It has over 4 millions titles, 8 millions casts and 250 million unique monthly users. ",br(),"IMDB 5000 Movie Dataset from Kaggle is a subset of the IMDB (Internet Movie Data Base). It consists of 5043 movies and 28 variables, spanning across 100 years."))
                        
                        ) 
              )
            ),
            fluidRow(
              column(width=5,
                     box(title="Variables", width=NULL,status= "primary", solidHeader=TRUE, height="120px",
                            selectInput("variable", "", colnames(overview_list), selected="director_name")),
                     box(title=textOutput("dataMes"), width=NULL, status= "primary", solidHeader=TRUE, height="253px",
                         verbatimTextOutput("varList"))
                     
              ),
              column(width=7,
                     box(title=textOutput("caption"), width=NULL, status= "primary", solidHeader=TRUE,
                         plotOutput("varPlot", height="330px"))
              )
              
            )),
    
    # ============ CO-STARRING NEWTORK ANALYSIS ============
    tabItem(tabName = "costar", 
            fluidRow(
              column(width=12, 
                      column(width=8,box(width=NULL, title="Co-starring Network", status= "primary", solidHeader=TRUE, forceNetworkOutput("rnet", height="305px")),
                             box(width=NULL,title="Top five highly connected actors", status= "primary", solidHeader=TRUE, height="200px",
                             #verbatimTextOutput("i_actors"))),
                             column(width=9,selectInput(inputId = "td_actor", label = "Select an actor for detail visual analytics", choices = "", selectize=FALSE, size=5)),
                             column(width=3,actionButton("search", "Search")))),
                      column(width=4,
                             box(width=NULL, title="Filters", status= "primary", solidHeader=TRUE,
                                             selectInput(inputId = "genre", label = "Select a genre", choices = c("All", genres_list$genre), selected="All"),
                                             sliderInput(inputId = "ratings", label = "IMDB ratings", min = 1.5, max = 9.5, value = c(7.5,9.5), step=0.5),
                                             sliderInput(inputId = "gross", label = "Gross in millions", min = as.integer(min(imdb_data$gross)/1000000), max = as.integer(max(imdb_data$gross)/1000000), value = c(as.integer(min(imdb_data$gross)/1000000), as.integer(max(imdb_data$gross)/1000000)), step=50),
                                             sliderInput(inputId = "years", label = "Years", min = min(imdb_data$title_year), max = max(imdb_data$title_year), value = c(min(imdb_data$title_year), max(imdb_data$title_year)), step=4),
                                             selectInput(inputId = "centrality", label = "Select Centrality", choices = list("Degree" = "degree", "Closeness" = "closeness", "Betweenness" = "betweenness"), selected = "betweenness"),
                                             selectInput(inputId = "community", label = "Select Clustering Algorithm", choices = list("Fast Greedy" = "fastgreedy", "Walktrap" = "walktrap", "Edge Betweenness" = "edgebetweenness"), selected = "fastgreedy")
                                         
                                  )
                             )
                                
                      ),
              bsModal(id="bsModal", title="Ego Network Visualisation", trigger="search", size="large",
                      tabsetPanel(type = "tabs",
                                  tabPanel("Collaboration pattern", textOutput("sbHeader"),
                                           tags$head(tags$style("#sbHeader{font-family: 'Trebuchet MS'; color: #666666; font-size: 20px;font-style: bold; background: #E6E6FA; text-align: center;}")),
                                           sunburstOutput("sunburst")),
                                  tabPanel("Genre Trends", textOutput("actHeader"),
                                           tags$head(tags$style("#actHeader{font-family: 'Trebuchet MS'; color: #666666; font-size: 20px;font-style: bold; background: #E6E6FA; text-align: center;}")),
                                           plotOutput("activeYear")),
                                  tabPanel("Network", textOutput("tdHeader"),
                                           tags$head(tags$style("#tdHeader{font-family: 'Trebuchet MS'; color: #666666; font-size: 20px;font-style: bold; background: #E6E6FA; text-align: center;}")),
                                           forceNetworkOutput("csnet", height="350px"),
                                           htmlOutput("dmHead"),
                                           tableOutput("dm")),
                                  tabPanel("Time Slice", textOutput("ggHeader"),
                                           tags$head(tags$style("#ggHeader{font-family: 'Trebuchet MS'; color: #666666; font-size: 20px;font-style: bold; background: #E6E6FA; text-align: center;}")),
                                           plotOutput("ggPlots"))
                                  
                                           
                      ))
                       
           
            )       
            
    ),
    
    
    #========== MOVIES ==============
    tabItem(tabName = "trend", 
            fluidRow(
              column(width=12,   
                     box(title="Movie Genre Trends", width=NULL, status= "primary", solidHeader=TRUE, 
                     fluidRow( 
                       column(width=12, title="Movies Visualisation", #height="280px", 
                              box(width=8, title="Genre Network Visualisation", status= "primary", solidHeader=TRUE, height="260px",
                                  fluidRow(
                                    column(width=12,plotOutput("gennet"), height="280px")) 
                                    #column(width=3, height="280px",actionButton("vgennet", "Zoom")))
                                  ),
                                 box(width=4, title="Filters", status= "primary", solidHeader=TRUE, height="260px",
                                  selectInput(inputId = "movie_genres", label = "Select a genre", choices = c("All", genres_list$genre), selected="All"),
                                  sliderInput(inputId = "movie_years", label = "Years", min = min(imdb_data$title_year), max = max(imdb_data$title_year), value = c(1980, max(imdb_data$title_year)), step=1)
                                  
                              ))

                     ),
                     fluidRow(
                       column(width=12,
                              box(width=8, title="Genre Temporal Trends", status= "primary", solidHeader=TRUE,
                                  plotOutput("stackPlot", height="200px")),
                              box(width=4, title="Genre Frequency", status= "primary", solidHeader=TRUE,
                                  plotOutput("freqPlot", height="200px")))
                                  #bubblesOutput("bubble")))
                       
                     ), 
                     bsModal("genresId", "Genre Network Visualisation", "vgennet", size="large", plotOutput("lgnet"))
                     ))
              )
            )
            
     )
)
            

shinyUI(
  dashboardPage(

    # skin
    skin = "blue",
    # Header
    header,
    # sidebar
    sidebar, 
    # body
    body)
  
)

