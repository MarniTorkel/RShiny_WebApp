library(dplyr)
library(tidyr)
library(shiny)
library(data.table)
library(rsconnect)


dataDir <- "./data/"

raw_IMDB <- fread(paste0(dataDir, "movie_metadata.csv"), na.strings=c("", " ", "NA"))
clean_IMDB <- na.omit(raw_IMDB)
IMDB <- unique(clean_IMDB)


