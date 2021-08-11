library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)
library(splitstackshape)


processFile <- function(folder, file){

  filename <- file
  text <- pdf_text(paste(folder, filename, sep="/"))
  text <- paste(text, collapse='')
  journal <- substr(filename, 14, 17)
  date <- as.Date(substr(filename, 5, 12), format="%Y%m%d")
  pattern <- "\\([^\\)]*\\)[^\\)][^à]*dép. [0-9]{2,3}"
  individuals <- regmatches(text, gregexpr(pattern, text, perl=T))[[1]]
  
  
  
  individuals <- gsub("[\n]", "", individuals)
  individuals <- gsub("[(]", "", individuals)
  individuals <- gsub("[)]", "", individuals)
  individuals <- gsub(" dép.", "", individuals)
  
  individuals <- data.frame(individuals, stringsAsFactors = FALSE)
  colnames(individuals) <- c("text")
  
  
  individualsCut <- cSplit(individuals, "text", ",")
  individualsCut <- cbind(individuals, individualsCut)
  
  #individualsCut <- individualsCut[,1:(length(individualsCut)-1)]
  
  names <- c("original", "country", "type", "idREZE", "department")
  colnames(individualsCut) <- names
  
  
  #postal code to France
  individualsCut$country <- as.character(individualsCut$country)
  individualsCut$country[grepl("[0-9]",individualsCut$country)] <- "France"
  individualsCut$country <- as.factor(individualsCut$country)
  
  #split REZE number into apYear, apSerie and apID
  
  individualsCut$idREZE <- as.character(individualsCut$idREZE)
  individualsCut$idREZE <- str_replace(individualsCut$idREZE, " ", "")
  
  apYearData <- data.frame(substring(individualsCut$idREZE, 1, 5))
  colnames(apYearData) <- "apYear"
  
  apSerieData <- data.frame(substring(individualsCut$idREZE, 6, 8))
  colnames(apSerieData) <- "apSerie"
  
  apIDData <- data.frame(substring(individualsCut$idREZE, 9, 11))
  colnames(apIDData) <- "apID"
  
  individualsCut <- cbind(individualsCut, apYearData, apSerieData, apIDData)
  
  
  #add date and journal id
  individualsCut  <- cbind(individualsCut, date, journal)
  
  
  return(individualsCut)
  
  
}








