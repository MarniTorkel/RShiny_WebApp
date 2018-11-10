library(dplyr)
library(tidyr)
library(shiny)
library(data.table)
library(rsconnect)


dataDir <- "./data/"

IMDB <- fread(paste0(dataDir, "movie_metadata.csv"), na.strings=c("", " ", "NA"))