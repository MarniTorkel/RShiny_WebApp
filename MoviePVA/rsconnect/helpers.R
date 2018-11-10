library(dplyr)
library(tidyr)
library(shiny)
library(data.table)
library(rsconnect)
#require(bit64)

dataDir <- "/Users/marnitorkel/Documents/COMP5703/Shiny /Project/MoviePVA/data/"

raw_IMDB <- fread(paste0(dataDir, "movie_metadata.csv"), na.strings=c("", " ", "NA"))
clean_IMDB <- na.omit(raw_IMDB)
IMDB <- unique(clean_IMDB)


