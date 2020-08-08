library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
require(ggrepel)
library(directlabels)
library(jpeg)
library(grid)
library(ggpubr)
library(ggimage)
library(zoo)
library(pdftools)
library(stringr)

fatalities_data <- data.frame(Death.No=character(),
                              Age=integer(),
                              Gender=character(),
                              Date.Of.Admission=character(),
                              Date.Of.Death=character(),
                              Reason=character(),
                              Hospital=character(),
                              stringsAsFactors = FALSE)

extract_data <- function(strObj) {
  strObj <- gsub("\\n"," ",strObj)
  strObj <- gsub("/",",",strObj)
  strObj <- gsub("Death case","",strObj, ignore.case = TRUE)
  
  
  case_no <- unlist(str_split(strObj,pattern=":"))[1]
  case_no <- str_extract_all(case_no, boundary("word"))[1]
  case_no <-  unlist(case_no)[1]
  
  age <- as.integer(str_match(tolower(strObj),"a (.*?) year")[2])
  age <-  gsub(",",".", age)
  
  gender <- toupper(sub(".*\\b(.*ale\\w*).*", "\\1", strObj))
  gender <-  gsub(",",".", gender)
  
  date_of_admission <- str_match(strObj,"admitted on (.*?) [a-zA-Z+]")[2]
  date_of_admission <- gsub(",",".", date_of_admission)
  
  date_of_death <- str_match(strObj,"died on (.*?) at")[2]
  date_of_admission <- gsub(",",".", date_of_admission)
  date_of_death <- gsub(",",".", date_of_death)
  
  reason_of_death <- str_match(strObj," due to ([a-zA-Z]+[[:space:]]+.*?)\\..*")[2]
  reason_of_death <- gsub(".","",reason_of_death, fixed=TRUE)
  reason_of_death <- gsub(",",";",reason_of_death, fixed=TRUE)
  
  hospital <- str_match(strObj," in ([^.]*)")[2]
  hospital <- paste(unlist(strsplit(hospital, split = " "))[1:6], collapse = " ")
  hospital <- gsub(","," ",hospital, fixed=TRUE)
  hospital <- gsub("NA"," ",hospital, fixed=TRUE)
  hospital <- unlist(str_extract_all(hospital,  boundary("word")))
  hospital <- paste(grep("^[A-Z]", hospital,  value = TRUE), collapse=" ")
  
  fatalities_data[nrow(fatalities_data)+1,] <<- c(case_no, age, gender, date_of_admission, date_of_death, reason_of_death, hospital)
}

setwd("~/Documents/datasciencecourse/github_root/covid19india-officialbulletins/tamil_nadu/")
files <- list.files(pattern = "pdf$")
for (i in files) {
  data <- as.list(pdf_text(i))
  lines_of_interest <- data[grep("Death Case", data)]
  if(length(lines_of_interest) > 0) {
    list_of_strings <- unlist(lapply(lines_of_interest, function(x){strsplit(x,split = "No.")}))
    strings <- list_of_strings[grep(list_of_strings, pattern="admitted")]
    for (iter in strings){
      extract_data(iter)
    }
  }
}

write.table(fatalities_data, file="~/Documents/datasciencecourse/github_root/TamilNaduCovidData/TamilNaduFatalitiesDataset.csv", sep=",", quote = FALSE, row.names = FALSE)

setwd("~/Documents/datasciencecourse/github_root/TamilNaduCovidData")

commit_msg <- paste("Adding data till", Sys.Date())
commit_cmd <- paste("git commit -m \"", commit_msg, "\"")
system("git add .")
system(commit_cmd)
system("git push")