source("functions.R")
library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)
library(splitstackshape)

mainExecute <- function (){
  folder <-"data/pdf_toprocess"
  files <- list.files(folder, pattern=NULL, all.files=FALSE, full.names=FALSE)
  dataTidy <- data.frame()
  
  
  for (file in files) {
    result <- processFile(folder, file)
    dataTidy <- rbind(dataTidy, result)
  }
  write.csv(dataTidy,"data/daily/dataTidy.csv")
  dataTidy <- dataTidy[,-c(1,4,8)]
  colnames(dataTidy) <- c("Birth Country","App. Type","App. Department","App. Year","App. Serie", "Publish Date", "Publish Journal")

  return(dataTidy)
  
}


