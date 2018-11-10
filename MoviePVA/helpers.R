# ==========================================================
# COMP5703 - IT CAPSTONE PROJECT - MOVIE VISUAL ANALYTICS
# ==========================================================

library(tm)
library(reshape2)
library(dplyr)
library(tidyr)
library(shiny)
library(data.table)
library(rsconnect)
#require(bit64)

dataDir <- "./data/"

imdb_raw <- read.csv(paste0(dataDir, "movie_metadata.csv"), na.strings="", stringsAsFactors = FALSE)
#imdb_raw <- read.csv('/Users/marnitorkel/Documents/COMP5703/Data/movie_metadata.csv', na.strings="", stringsAsFactors = FALSE)
imdb_clean <- na.omit(imdb_raw)
imdb_data <- unique(imdb_clean)

# Extract words without stopwords
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# Split the words into individual word
# tData <- function(colData)
#   docs <- Corpus(VectorSource(colData))
#   # Convert the text to lower case
#   docs <- tm_map(docs, content_transformer(tolower))
#   # Remove english common stopwords
#   docs <- tm_map(docs, removeWords, stopwords("english"))
#   # Remove punctuations
#   docs <- tm_map(docs, removePunctuation)
#   # Eliminate extra white spaces
#   docs <- tm_map(docs, stripWhitespace)
#   dtm <- TermDocumentMatrix(docs)
#   m <- as.matrix(dtm)
#   v <- sort(rowSums(m),decreasing=TRUE)
#   d <- data.frame(word = names(v),freq=v)
#   return(d)

# =============ACTORS============
# # Only extract movies with English language
# data_english <- subset(data, data['language'] == "English")
# # actors collaboration
# actors_english_df <- data.frame(data_english$movie_title, data_english$actor_1_name, data_english$actor_2_name, data_english$actor_3_name)
# 
# data_years_df <- data.frame(data_english$movie_title, data_english$title_year)
# names(data_years_df) <- c("movie_title", "title_year")
# 
# # generate nodes
# co_actors_english_df <- melt(actors_english_df, id.vars = "data_english.movie_title")
# names(co_actors_english_df) <- c("movie_title", "a_num", "actor")
# 
# actors_years <- merge(co_actors_english_df , data_years_df, by = "movie_title")
# 
# actors_nodes <- data.frame(actors_years[,3],actors_years[,3], actors_years[,4])
# names(actors_nodes) <- c("id", "label", "year")
# actors_nodes <- unique(actors_nodes)
# 
# #write.csv(actors_nodes, file = '/Users/marnitorkel/Documents/COMP5703/data/actors_nodes.csv', row.names = F)
# 
# # generate edges
# edges <- actors_years[,c(1,3)]
# a_edges <- merge(x=edges, y=edges, by="movie_title")
# a_edges <- subset(a_edges, a_edges$actor.x != a_edges$actor.y)
# actors_edges <- a_edges[, c(2,3)]
# 
# names(actors_edges) <- c("source", "target")
# 
# #write.csv(actors_edges, file = '/Users/marnitorkel/Documents/COMP5703/data/actors_edges.csv', row.names = F)
# 
