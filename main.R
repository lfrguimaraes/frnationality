setwd("~/frnationality")
source("functions.r")

folder <-"data"
files <- list.files(folder, pattern=NULL, all.files=FALSE, full.names=FALSE)
dataTidy <- data.frame()

for (file in files) {
  result <- processFile(folder, file)
  dataTidy <- rbind(dataTidy, result)
}


