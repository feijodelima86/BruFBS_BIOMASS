library(openxlsx)
library(tidyverse)
library(readxl)

#CALCULATED SPREADSHEETS FOLDER

path<-"2_Incremental/2_Calculation/"

#LIST OF SPREADSHEETS REQUIRED

INTEGRATION.LIST<-c(list.files(path= path, full.names=TRUE,pattern=c("RESULT")))

#TURN ALL SPREADSHEETS INTO A LIST OF OBJECTS

All<-lapply(INTEGRATION.LIST,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.csv(filename, stringsAsFactors=FALSE)
})


#MERGE SPREADSHEETS BY DESIRED CATEGORIES


INTEGRATED<-All%>%reduce(full_join, by = c("PROJECT","SAMPLE","SITE","DATE","SUBSTRATE","AVERAGE.DEPTH"))

INTEGRATED$DATE<-as.Date(INTEGRATED$DATE, origin = "1899-12-30")


write.csv(INTEGRATED,"2_Incremental/3_Integration/INTEGRATED_CURRENT.csv")
  
