library(openxlsx)
library(tidyverse)
library(readxl)
######EPILITHON CALCULATIONS#######

#DIGITALIZED SPREADSHEETS FOLDER

path<-"2_Incremental/1_Digitalization/RASS/"

#LIST OF SPREADSHEETS REQUIRED

#c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON")),

EPIL.LIST.FIELD<-c(list.files(path= path, full.names=TRUE,pattern=c("FIELD")))

FIELD<-lapply(EPIL.LIST.FIELD,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})

EPIL.FIELD.SAMPLE<-FIELD%>%reduce(full_join)

EPIL.FIELD.SAMPLE$SAMPLE<-as.numeric(factor(EPIL.FIELD.SAMPLE$SAMPLE))

write.xlsx(EPIL.FIELD.SAMPLE,"2_Incremental/1_digitalization/RASS/RASS_BIOMASS_FIELD_RUNNING_DIG.xlsx",
          row.names = FALSE)

###################################################################################################

EPIL.LIST.AFDM<-c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON_AFDM")))

EPIL.AFDM<-lapply(EPIL.LIST.AFDM,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})

EPIL.AFDM.SAMPLE<-EPIL.AFDM%>%reduce(full_join)

EPIL.AFDM.SAMPLE$SAMPLE<-as.numeric(factor(EPIL.AFDM.SAMPLE$SAMPLE))

write.xlsx(EPIL.AFDM.SAMPLE,"2_Incremental/1_digitalization/RASS/RASS_EPILITHON_AFDM_RUNNING_DIG.xlsx",
          row.names = FALSE)

#####################################################################################################


EPIL.LIST.PIGMENTS<-c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON_PIGMENTS")))

EPIL.PIGMENTS<-lapply(EPIL.LIST.PIGMENTS,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})

EPIL.PIGMENTS.SAMPLE<-EPIL.PIGMENTS%>%reduce(full_join)

EPIL.PIGMENTS.SAMPLE$SAMPLE<-as.numeric(factor(EPIL.PIGMENTS.SAMPLE$SAMPLE))

write.xlsx(EPIL.PIGMENTS.SAMPLE,"2_Incremental/1_digitalization/RASS/RASS_EPILITHON_PIGMENTS_RUNNING_DIG.xlsx",
          row.names = FALSE)


####################################################################################################

FBOM.LIST.AFDM<-c(list.files(path= path, full.names=TRUE,pattern=c("FBOM_AFDM")))

FBOM.AFDM<-lapply(FBOM.LIST.AFDM,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})

FBOM.AFDM.SAMPLE<-FBOM.AFDM%>%reduce(full_join)

FBOM.AFDM.SAMPLE$SAMPLE<-as.numeric(factor(FBOM.AFDM.SAMPLE$SAMPLE))

write.xlsx(FBOM.AFDM.SAMPLE,"2_Incremental/1_digitalization/RASS/RASS_FBOM_AFDM_RUNNING_DIG.xlsx",
          row.names = FALSE)


####################################################################################################

FBOM.LIST.PIGMENTS<-c(list.files(path= path, full.names=TRUE,pattern=c("FBOM_PIGMENTS")))

FBOM.PIGMENTS<-lapply(FBOM.LIST.PIGMENTS,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})

FBOM.PIGMENTS.SAMPLE<-FBOM.PIGMENTS%>%reduce(full_join)

FBOM.PIGMENTS.SAMPLE$SAMPLE<-as.numeric(factor(FBOM.PIGMENTS.SAMPLE$SAMPLE))

write.xlsx(FBOM.PIGMENTS.SAMPLE,"2_Incremental/1_digitalization/RASS/RASS_FBOM_PIGMENTS_RUNNING_DIG.xlsx",
          row.names = FALSE)


####################################################################################################

