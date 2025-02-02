
library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)
library(splitstackshape)
library(bizdays)
library(logr)
library(filesstrings)

configFolderToProcess <- "data/pdf_toprocess"
configFolderProcessed <- "data/pdf_processed"
configFolderLog <- "log"
configFolderTidy <- "data/daily"
configTidyFileName <- "daily.csv"


addLog <- function(textLog, folderLog=NULL){
  library(logr)
  if(is.null(folderLog))folderLog <- "log"
  
  filepath <- str_c(folderLog,"LOG.txt", sep="/")
  textLog <- str_c(date(),textLog, sep="-")
  
  logFile <- log_open(filepath)
  log_print(textLog)
  log_close()
  
}

getFilesToProcess <- function(folder=NULL){
  folderToProcess <- "data/pdf_toprocess"
  filesToProcess <- list.files(folderToProcess, pattern=NULL, all.files=FALSE, full.names=FALSE)
  
}

getFilesProcessed <- function(folder=NULL){
  
  folderTidy <- "data/daily"
  fileTidyPath <- str_c(folderTidy,"daily.csv", sep="/") 
  dataTidyFile <- read.csv(fileTidyPath, colClasses=c("character","character","factor","factor","character","character","factor","factor","character","factor","factor","character","character"))
  dataTidyFile <- dataTidyFile[,c(12,13)]
  dataTidyFile <- dataTidyFile[!duplicated(dataTidyFile$filename),]

  
  return (dataTidyFile)

  
}
  
  
reprocess <- function(folderToProcess=NULL, folderProcessed=NULL, folderTidy=NULL){
  

  folderToProcess <- "data/pdf_toprocess"
  folderProcessed <- "data/pdf_processed"
  folderTidy <- "data/daily"
  fileTidyPath <- str_c(folderTidy,"daily.csv", sep="/") 
  

  
  filesToProcess <- list.files(folderToProcess, pattern=NULL, all.files=FALSE, full.names=FALSE)
  
  tidyFile <- list.files(folderTidy, pattern=NULL, all.files=FALSE, full.names=FALSE)
  
  dataTidy <- data.frame()
  
  if(length(filesToProcess)>=1){
    for (file in filesToProcess) {
      result <- processFile(folderToProcess, file)
      addLog(str_c("File processed: ",file, ". ", nrow(result), " lines processed.", sep =""))
      filePathToProcess <- str_c(folderToProcess,file, sep="/")
      file.remove(filePathToProcess)
      dataTidy <- rbind(dataTidy, result)
    }
    
  }else addLog("No new files to process")
  
  if(hasTidyDataFile()){
    fileTidyPath <- str_c(folderTidy,tidyFile, sep="/")
    dataTidyFile <- read.csv(fileTidyPath, colClasses=c("character","character","factor","factor","character","character","factor","factor","character","factor","factor","character","character"))
    dataTidyFile <- dataTidyFile[,-c(1)]
    dataTidy <- rbind(dataTidy, dataTidyFile)
    
  }
  
  if(length(dataTidy)>0){
    duplicates <- data.frame()
    duplicates <- data.frame(duplicated(dataTidy$original))
    colnames(duplicates) <- c("checkDuplicates")
    addLog(str_c("Removed duplicates from data frame.", length(duplicates$checkDuplicates[duplicates$checkDuplicates == TRUE]), " lines removed", sep =""))
    dataTidy <- dataTidy[!duplicated(dataTidy$original),]
    
    write.csv(dataTidy, fileTidyPath)
  }



}

getTidyData <- function(file=NULL){
  folderTidy <- "data/daily"
  fileTidyPath <- str_c(folderTidy,"daily.csv", sep="/") 
  dataTidyFile <- read.csv(fileTidyPath, colClasses=c("character","character","factor","factor","character","character","factor","factor","character","factor","factor","character","character"))
  dataTidyFile <- dataTidyFile[,-c(1,2,5,9,12,13)]
  colnames(dataTidyFile) <- c("Country","Type","Department","Year","Serie", "Publish Date", "Journal")
  
  return (dataTidyFile)
  
}

hasTidyDataFile <- function(file=NULL){
  folderTidy <- "data/daily"
  fileTidyPath <- str_c(folderTidy,"daily.csv", sep="/") 
  tidyFile <- list.files(folderTidy, pattern=NULL, all.files=FALSE, full.names=FALSE)
  
  if(length(tidyFile)>0) return (TRUE)
  else return (FALSE)
}
  
  
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

  apYearData <- data.frame(substring(individualsCut$idREZE, 1, 4))
  colnames(apYearData) <- "apYear"
  
  apSerieData <- data.frame(substring(individualsCut$idREZE, 6, 8))
  colnames(apSerieData) <- "apSerie"
  
  apIDData <- data.frame(substring(individualsCut$idREZE, 9, 11))
  colnames(apIDData) <- "apID"
  
  individualsCut <- cbind(individualsCut, apYearData, apSerieData, apIDData)
  
  
  #add date and journal id
  processDate <-date()
  individualsCut  <- cbind(individualsCut, date, journal, filename, processDate)
  
  #convert to factors
  individualsCut$apSerie <- as.factor(individualsCut$apSerie)
  individualsCut$apYear <- as.factor(individualsCut$apYear)
  individualsCut$country <- as.factor(individualsCut$country)
  individualsCut$type <- as.factor(individualsCut$type)
  individualsCut$date <- as.factor(individualsCut$date)
  individualsCut$journal <- as.factor(individualsCut$journal)
  
  
  
  individualsCut
  
  
}








