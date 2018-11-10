# =================================================================
# COMP5703 - IT CAPSTONE PROJECT - SCALABLE MOVIE VISUAL ANALYTICS
# =================================================================
# Author: Marni Torkel
# Codes are based on tutorial from the following:
# Github, Kaggles, R Studio, Kateto.net, Stackoverflow, etc
# ================================================================
#
library(NLP)
library(tm)
library(reshape2)
library(dplyr)
library(tidyr)
library(shiny)
library(data.table)
library(rsconnect)
library(sigma)
library(rgexf)
#require(bit64)

dataDir <- "./data/"

imdb_raw <- read.csv(paste0(dataDir, "movie_metadata.csv"), na.strings="", stringsAsFactors = FALSE)
#imdb_raw <- read.csv('/Users/marnitorkel/Documents/COMP5703/Shiny/Project/MoviePVA/data/movie_metadata.csv', na.strings="", stringsAsFactors = FALSE)
imdb_clean <- na.omit(imdb_raw)
imdb_data <- unique(imdb_clean)

overview_list <- data.frame(imdb_data$director_name, imdb_data$actor_1_name, imdb_data$actor_2_name, imdb_data$actor_3_name, imdb_data$gross, imdb_data$genres, imdb_data$movie_title, imdb_data$plot_keywords, imdb_data$language, imdb_data$country, imdb_data$title_year, imdb_data$imdb_score)
names(overview_list) <- gsub("imdb_data.", "", names(overview_list), fixed = TRUE)
#overview_list <- c("Actors", "Directors", "Director Average Ratings","Director Average Revenue","Revenue in years", "IMDB Ratings", "Genres", "Title years")
movie_genre_year <- c("1927-2016", "1927-1989", "1990-1999", "2000-2009", "2010-2016")

genres_list <- unique(as.data.frame(sort(unlist(strsplit(as.character(imdb_data$genres), split="\\|"))), stringsAsFactors = FALSE))
rownames(genres_list) <- NULL
colnames(genres_list) <- "genre"

getNL <- function(ratings, years, genre, gross) {
  nl <- c()
  minGross <- as.integer(gross[1] * 1000000)
  maxGross <- as.integer(gross[2] * 1000000)
  # Get dataset subset
  actors_data <- subset(imdb_data, imdb_data$imdb_score >= ratings[1] &
                          imdb_data$imdb_score <= ratings[2] &
                          imdb_data$title_year >= years[1] &
                          imdb_data$title_year <= years[2] &
                          imdb_data$gross >= minGross &
                          imdb_data$gross <= maxGross &
                          imdb_data$language == "English")
  if (!str_detect(genre, "All")) {
    actors_data <- actors_data[grep(genre, actors_data$genres),]}
  # Generate movies and actors data frame
  actors_df <- data.frame(actors_data$movie_title, actors_data$actor_1_name, actors_data$actor_2_name, actors_data$actor_3_name)
  # co-actors
  co_actors_df <- unique(melt(actors_df, id.vars = "actors_data.movie_title"))
  names(co_actors_df) <- c("movie_title", "a_num", "actor")
  actors_names <- as.data.frame(co_actors_df[,3], stringsAsFactors = FALSE)
  actors_names <- unique(actors_names)
  actors_names <- actors_names[ order(actors_names[,1]), ]
  actors_id <- 0:(length(actors_names)-1)
  # nodes
  actors_nodes <- data.frame(actors_id, actors_names)
  names(actors_nodes) <- c("id", "name")
  #actors_nodes$group <- rep(1,nrow(actors_nodes))
  #actors_nodes$size <- rep(1,nrow(actors_nodes))
  #links
  rlinks <- co_actors_df[,c(1,3)]
  names(rlinks) <- c("movie_title", "name")
  # change name column 2 to id as from actors_nodes
  links_id <- data.frame(movie_title=rlinks$movie_title, id=actors_nodes[match(rlinks$name, actors_nodes$name), 1])
  a_links <- merge(x=links_id, y=links_id, by="movie_title")
  a_links <- subset(a_links, a_links$id.x != a_links$id.y)
  actors_links <- a_links[, c(2,3)]
  # sort links
  actors_links <- actors_links[ order(actors_links[,1], actors_links[,2]), ]
  actors_links$values <- rep(1,nrow(actors_links))
  actors_links <- aggregate(actors_links[,3], actors_links[,-3], sum)
  actors_links <- actors_links[ order(actors_links[,1], actors_links[,2]), ]
  names(actors_links) <- c("from", "to", "value")
  rownames(actors_links) <- NULL
  nl$nodes <- actors_nodes
  nl$links <- actors_links
  return(nl)
  
}

getActor <- function(td_actor) {
  # Get actor subset
  td_data <- imdb_data[imdb_data$actor_1_name == td_actor |
                         imdb_data$actor_2_name == td_actor |
                         imdb_data$actor_3_name == td_actor , ]
  
  tda <- na.omit(data.frame(td_data$movie_title, td_data$director_name, td_data$actor_1_name, td_data$actor_2_name, td_data$actor_3_name, 
                         td_data$genres, td_data$title_year, td_data$imdb_score, td_data$gross, td_data$budget))
  return(tda)
  
}

actorList <- unique(data.frame(c(imdb_data$actor_1_name, imdb_data$actor_2_name, imdb_data$actor_3_name)))
colnames(actorList) <- "name"
actorList <- data.frame(actorList[order(actorList$name),], stringsAsFactors = FALSE)
colnames(actorList) <- "name"
