library(openxlsx)
library(tidyverse)
library(readxl)
library(tibble)

path<-"2_Incremental/3_Integration/"

#LIST OF SPREADSHEETS REQUIRED

INTEGRATION.LIST<-c(list.files(path= path, full.names=TRUE,pattern=c("INTEGRATED")))

BIOMASS.ALL<-lapply(INTEGRATION.LIST,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.csv(filename)
})

BIOMASS.ISF<-data.frame(BIOMASS.ALL%>%reduce(full_join))

colnames(BIOMASS.ISF)

names(BIOMASS.ISF)[names(BIOMASS.ISF) == 'X'] <- 'ROW'

colnames(BIOMASS.ISF)<-gsub("\\.", "_", colnames(BIOMASS.ISF))

BIOMASS.ISF<-add_column(BIOMASS.ISF, YEAR=format(as.Date(BIOMASS.ISF$DATE), "%Y"), .after = "DATE")
BIOMASS.ISF<-add_column(BIOMASS.ISF, MONTH=format(as.Date(BIOMASS.ISF$DATE), "%m"), .after = "DATE")
BIOMASS.ISF<-add_column(BIOMASS.ISF, DAY=format(as.Date(BIOMASS.ISF$DATE), "%d"), .after = "DATE")

write.csv(BIOMASS.ISF,"2_Incremental/5_ISF_Generation/BIOMASS_ISF.csv")