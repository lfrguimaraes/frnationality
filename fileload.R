setwd("~/frnationality")
library(pdftools)
folder <-"data"
filename <-"joe_20210808_0183_c000.pdf"
text <- pdf_text(paste(folder, filename, sep="/"))
text <- paste(text, collapse='')
date <- as.Date(substr(filename, 5, 12), format="%Y%m%d")
pattern <- "\\([^\\)]*\\)[^\\)]*dép. [0-9]{2,3}"
individuals <- regmatches(text, gregexpr(pattern, text, perl=T))[[1]]
individuals <- gsub("[\n]", "", individuals)
individuals <- gsub("[(]", "", individuals)
individuals <- gsub("[)]", "", individuals)
individuals <- gsub(" dép.", "", individuals)

