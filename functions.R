library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)
library(splitstackshape)


processFile <- function(folder, file){

  options(encoding = "UTF-8")
  filename <- file
  text <- pdf_text(paste(folder, filename, sep="/"))
  text <- paste(text, collapse='')
  journal <- substr(filename, 14, 17)
  date <- substr(filename, 5, 12)
  
  pattern <- "\\([^\\)]*\\)[^\\)][^à]*dép. [0-9]{2,3}"
  individuals <- regmatches(text, gregexpr(pattern, text, perl=T))[[1]]

  individuals <- gsub("[\n]", "", individuals)
  individuals <- gsub("[(]", "", individuals)
  individuals <- gsub("[)]", "", individuals)
  individuals <- gsub(" dép.", "", individuals)
  
  #create data frame
  individuals <- data.frame(individuals, stringsAsFactors = FALSE)
  colnames(individuals) <- c("text")
 
  
  #split text by , and create new data frame
  individualsCut <- separate(data = individuals, col = text, into = c("text_1", "text_2","text_3","text_4"), sep = ",")
  individualsCut <- cbind(individuals, individualsCut)
  
  
  names <- c("original", "country", "type", "idREZE", "department")
  colnames(individualsCut) <- names

  
  #remove left and right blank spaces from "type", "idREZE", "department"

  individualsCut$type <- gsub(" ","",as.character(individualsCut$type))
  individualsCut$idREZE <- gsub(" ","",as.character(individualsCut$idREZE))
  individualsCut$department <- gsub(" ","",as.character(individualsCut$department))
  
  #postal code to France
  individualsCut$country <- as.character(individualsCut$country)
  individualsCut$country[grepl("[0-9]",individualsCut$country)] <- "France"
  individualsCut$country <- as.factor(individualsCut$country)
  
  #split REZE number into apYear, apSerie and apID
  
  individualsCut$idREZE <- as.character(individualsCut$idREZE)

  apYearData <- data.frame(substring(individualsCut$idREZE, 1, 5))
  colnames(apYearData) <- "apYear"
  
  apSerieData <- data.frame(substring(individualsCut$idREZE, 6, 8))
  colnames(apSerieData) <- "apSerie"
  
  apIDData <- data.frame(substring(individualsCut$idREZE, 9, 11))
  colnames(apIDData) <- "apID"
  
  individualsCut <- cbind(individualsCut, apYearData, apSerieData, apIDData)
  
  
  #add date and journal id
  individualsCut  <- cbind(individualsCut, date, journal)
  
  #convert to factors
  individualsCut$apSerie <- as.factor(individualsCut$apSerie)
  individualsCut$apYear <- as.factor(individualsCut$apYear)
  individualsCut$country <- as.factor(individualsCut$country)
  individualsCut$type <- as.factor(individualsCut$type)
  individualsCut$date <- as.factor(individualsCut$date)
  individualsCut$journal <- as.factor(individualsCut$journal)
  
  
  
  individualsCut
  
  
}








