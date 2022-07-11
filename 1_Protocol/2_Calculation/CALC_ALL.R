library(openxlsx)
library(tidyverse)
library(readxl)
######EPILITHON CALCULATIONS#######

#DIGITALIZED SPREADSHEETS FOLDER

path<-"2_Incremental/1_Digitalization/"

#LIST OF SPREADSHEETS REQUIRED

#c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON")),

EPIL.LIST.FIELD<-c(list.files(path= path, full.names=TRUE,pattern=c("FIELD")))

EPIL.LIST.AFDM<-c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON_AFDM")))

file.remove("2_Incremental/1_Digitalization/~$RASS_EPILITHON_AFDM_RUNNING_DIG.xlsx")

EPIL.LIST.PIGMENTS<-c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON_PIGMENTS")))

EPIL.METALS<-NA

#TURN ALL SPREADSHEETS INTO A LIST OF OBJECTS

FIELD<-lapply(EPIL.LIST.FIELD,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


AFDM<-lapply(EPIL.LIST.AFDM,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})

PIGMENTS<-lapply(EPIL.LIST.PIGMENTS,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


#MERGE SPREADSHEETS BY DESIRED CATEGORIES

EPIL.FIELD.CALC<-FIELD%>%reduce(full_join)

EPIL.AFDM.CALC<-AFDM%>%reduce(full_join)

EPIL.PIGMENTS.CALC<-PIGMENTS%>%reduce(full_join)

All<-list(EPIL.AFDM.CALC,EPIL.PIGMENTS.CALC,EPIL.FIELD.CALC)

EPIL.CALC<-All%>%reduce(full_join, by = c("PROJECT","DATE","SITE","SAMPLE"))


#PERFORM ALL CALCULATIONS. DOUBLE CHECKED WITH THE CALC SPREADSHEETS. 

EPIL.CALC$AVERAGE.DEPTH<-(EPIL.CALC$"DEPTH.1.(cm)"+EPIL.CALC$"DEPTH.2.(cm)"+EPIL.CALC$"DEPTH.3.(cm)"+EPIL.CALC$"DEPTH.4.(cm)")/4
EPIL.CALC$SRAD<-16.5
EPIL.CALC$SAMPLER.AREA<-3.14159226*EPIL.CALC$SRAD^2
EPIL.CALC$EPIL.DRY.ASH.ON.FILTER<-EPIL.CALC$"FILTERED.AFDM.(g)"-EPIL.CALC$"FILTER.WEIGHT.(g)"
EPIL.CALC$EPIL.OM.ON.FILTER<-EPIL.CALC$"FILTERED.DRY.WEIGHT.(g)"-EPIL.CALC$"FILTERED.AFDM.(g)"
EPIL.CALC$EPIL.BIOMASS.FILTERED.RATIO<-EPIL.CALC$"FILTERED.VOLUME.(mL)"/EPIL.CALC$"SAMPLE.VOLUME.(mL)"
EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.BIOMASS<-EPIL.CALC$"FILTERED.VOLUME.(mL)"/EPIL.CALC$"TOTAL.VOLUME.OF.SAMPLE.(ml)"*EPIL.CALC$SAMPLER.AREA/10000
EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS<-EPIL.CALC$"VOLUME.OF.SAMPLE.FILTERED.(ml)"/EPIL.CALC$"TOTAL.VOLUME.OF.SAMPLE.(ml)"*EPIL.CALC$SAMPLER.AREA/10000
EPIL.CALC$EPIL.OM.AREA.g.m2<-EPIL.CALC$EPIL.OM.ON.FILTER/EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.BIOMASS
EPIL.CALC$EPIL.ASH.AREA.g.m2<-EPIL.CALC$EPIL.DRY.ASH.ON.FILTER/EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.BIOMASS
EPIL.CALC$EPIL.ORGANIC.PROPORTION<-EPIL.CALC$"EPIL.OM.AREA.g.m2"/(EPIL.CALC$"EPIL.OM.AREA.g.m2"+EPIL.CALC$"EPIL.ASH.AREA.g.m2")
EPIL.CALC$EPIL.CARBON.MASS<-EPIL.CALC$EPIL.OM.AREA.g.m2*0.52
EPIL.CALC$EPIL.CHLA.MG.M2.HAUER<-26.7*((EPIL.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-(EPIL.CALC$`Absorbance.at.665.(post.acid)`-EPIL.CALC$`Absorbance.at.750.(post.acid)`))*EPIL.CALC$`VOLUME.OF.ACETONE.USED.(L)`/EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS
EPIL.CALC$EPIL.PHAEO.MG.M2.HAUER<-26.7*((1.7*EPIL.CALC$`Absorbance.at.665.(post.acid)`-EPIL.CALC$`Absorbance.at.750.(post.acid)`)-(EPIL.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*EPIL.CALC$`VOLUME.OF.ACETONE.USED.(L)`/EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS
EPIL.CALC$"EPIL.640/664"<-EPIL.CALC$`Absorbance.at.470.nm.(pre.acid)`/EPIL.CALC$`Absorbance.at.664.nm.(pre.acid)`
EPIL.CALC$`EPIL.PHICOCYANIN.MG/M2`<-(EPIL.CALC$`PHICOCYANIN.(ppb).FINAL`/10^6)*EPIL.CALC$"DILLUTION.FACTOR.PHYCO"*EPIL.CALC$"TOTAL.VOLUME.OF.SAMPLE.(ml)"/(EPIL.CALC$SAMPLER.AREA/10000)
EPIL.CALC$EPIL.CHLA.MG.M2.RITCHIE<-(-0.3319*(EPIL.CALC$`Absorbance.at.630.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.7485*(EPIL.CALC$`Absorbance.at.647.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+11.9442*(EPIL.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.4306*(EPIL.CALC$`Absorbance.at.691.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(EPIL.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
EPIL.CALC$EPIL.CHLB.MG.M2.RITCHIE<-(-1.2825*(EPIL.CALC$`Absorbance.at.630.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+19.8839*(EPIL.CALC$`Absorbance.at.647.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-4.886*(EPIL.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-2.3416*(EPIL.CALC$`Absorbance.at.691.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(EPIL.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
EPIL.CALC$EPIL.CHLC.MG.M2.RITCHIE<-(23.5902*(EPIL.CALC$`Absorbance.at.630.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-7.8516*(EPIL.CALC$`Absorbance.at.647.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.5214*(EPIL.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.7443*(EPIL.CALC$`Absorbance.at.691.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(EPIL.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
EPIL.CALC$EPIL.CHLD.MG.M2.RITCHIE<-(-0.5881*(EPIL.CALC$`Absorbance.at.630.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+0.0902*(EPIL.CALC$`Absorbance.at.647.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-0.1564*(EPIL.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+11.0473*(EPIL.CALC$`Absorbance.at.691.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(EPIL.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
EPIL.CALC$EPIL.CHL.TOTAL.MG.M2.RITCHIE<-(21.3877*(EPIL.CALC$`Absorbance.at.630.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+10.3739*(EPIL.CALC$`Absorbance.at.647.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+5.3805*(EPIL.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+5.5309*(EPIL.CALC$`Absorbance.at.691.nm.(pre.acid)`-EPIL.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(EPIL.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(EPIL.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)

# DESIRED VARIABLES EXTRACTED INTO RESULTS FILE

EPIL.RESULT<-data.frame(cbind(EPIL.CALC$"PROJECT",
                              EPIL.CALC$"DATE",
                              EPIL.CALC$"SITE",
                              EPIL.CALC$"SAMPLE",
                              EPIL.CALC$"SUBSTRATE",
                              EPIL.CALC$"AVERAGE.DEPTH",
                              EPIL.CALC$"EPIL.OM.AREA.g.m2",
                              EPIL.CALC$"EPIL.ASH.AREA.g.m2",
                              EPIL.CALC$"EPIL.CHLA.MG.M2.HAUER",
                              EPIL.CALC$"EPIL.PHAEO.MG.M2.HAUER",
                              EPIL.CALC$"EPIL.640/664",
                              EPIL.CALC$"EPIL.PHICOCYANIN.MG/M2",
                              EPIL.CALC$"EPIL.CHLA.MG.M2.RITCHIE",
                              EPIL.CALC$"EPIL.CHLB.MG.M2.RITCHIE",
                              EPIL.CALC$"EPIL.CHLC.MG.M2.RITCHIE",
                              EPIL.CALC$"EPIL.CHLD.MG.M2.RITCHIE",
                              EPIL.CALC$"EPIL.CHL.TOTAL.MG.M2.RITCHIE"))

# NAMING COLUMNS ON FINAL PRODUCT

names(EPIL.RESULT)<-cbind("PROJECT",
                          "DATE",
                          "SITE",
                          "SAMPLE",
                          "SUBSTRATE",
                          "AVERAGE.DEPTH",
                          "EPIL.OM.AREA.g.m2",
                          "EPIL.ASH.AREA.g.m2",
                          "EPIL.CHLA.MG.M2.HAUER",
                          "EPIL.PHAEO.MG.M2.HAUER", 
                          "EPIL.640/664",
                          "PHICOCYANIN.MG/M2",
                          "EPIL.CHLA.MG.M2.RITCHIE",
                          "EPIL.CHLB.MG.M2.RITCHIE",
                          "EPIL.CHLC.MG.M2.RITCHIE",
                          "EPIL.CHLD.MG.M2.RITCHIE",
                          "EPIL.CHL.TOTAL.MG.M2.RITCHIE")

# EXPORT CSV INTO INCREMENTAL FOLDER

write.csv(EPIL.RESULT,"2_Incremental/2_Calculation/EPIL.RESULT.csv",
          row.names = FALSE)

# REMOVE ALL OBJECTS TO AVOID CONFLICT WITH THE NEXT COMPARTMENT CALCULATIONS

rm(list=setdiff(ls(), "path"))

######EPIPHYTHON CALCULATIONS#######

#LIST OF SPREADSHEETS REQUIRED

#c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON")),

EPIP.LIST.FIELD<-c(list.files(path= path, full.names=TRUE,pattern=c("FIELD")))

EPIP.LIST.AFDM<-c(list.files(path= path, full.names=TRUE,pattern=c("EPIPHYTON_AFDM")))

EPIP.LIST.PIGMENTS<-c(list.files(path= path, full.names=TRUE,pattern=c("EPIPHYTON_PIGMENTS")))

#TURN ALL SPREADSHEETS INTO A LIST OF OBJECTS

FIELD<-lapply(EPIP.LIST.FIELD,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


AFDM<-lapply(EPIP.LIST.AFDM,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})

PIGMENTS<-lapply(EPIP.LIST.PIGMENTS,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


#MERGE SPREADSHEETS BY DESIRED CATEGORIES

EPIP.FIELD.CALC<-FIELD%>%reduce(full_join)

EPIP.AFDM.CALC<-AFDM%>%reduce(full_join)

EPIP.PIGMENTS.CALC<-PIGMENTS%>%reduce(full_join)

All<-list(EPIP.AFDM.CALC,EPIP.PIGMENTS.CALC,EPIP.FIELD.CALC)

EPIP.CALC<-All%>%reduce(full_join, by = c("PROJECT","DATE","SITE","SAMPLE"))


EPIP.CALC$AVERAGE.DEPTH<-(EPIP.CALC$"DEPTH.1.(cm)"+EPIP.CALC$"DEPTH.2.(cm)"+EPIP.CALC$"DEPTH.3.(cm)"+EPIP.CALC$"DEPTH.4.(cm)")/4
EPIP.CALC$SRAD<-16.5
EPIP.CALC$SAMPLER.AREA<-3.14159226*EPIP.CALC$SRAD^2
EPIP.CALC$EPIP.DRY.ASH.ON.FILTER<-EPIP.CALC$"FILTERED.AFDM.(g)"-EPIP.CALC$"FILTER.WEIGHT.(g)"
EPIP.CALC$EPIP.OM.ON.FILTER<-EPIP.CALC$"FILTERED.DRY.WEIGHT.(g)"-EPIP.CALC$"FILTERED.AFDM.(g)"
EPIP.CALC$EPIP.BIOMASS.FILTERED.RATIO<-EPIP.CALC$"FILTERED.VOLUME.(mL)"/EPIP.CALC$"SAMPLE.VOLUME.(mL)"
EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.BIOMASS<-EPIP.CALC$"FILTERED.VOLUME.(mL)"/EPIP.CALC$"TOTAL.VOLUME.OF.SAMPLE.(ml)"*EPIP.CALC$SAMPLER.AREA/10000
EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS<-EPIP.CALC$"VOLUME.OF.SAMPLE.FILTERED.(ml)"/EPIP.CALC$"TOTAL.VOLUME.OF.SAMPLE.(ml)"*EPIP.CALC$SAMPLER.AREA/10000
EPIP.CALC$EPIP.OM.AREA.g.m2<-EPIP.CALC$EPIP.OM.ON.FILTER/EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.BIOMASS
EPIP.CALC$EPIP.ASH.AREA.g.m2<-EPIP.CALC$EPIP.DRY.ASH.ON.FILTER/EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.BIOMASS
EPIP.CALC$EPIP.ORGANIC.PROPORTION<-EPIP.CALC$"EPIP.OM.AREA.g.m2"/(EPIP.CALC$"EPIP.OM.AREA.g.m2"+EPIP.CALC$"EPIP.ASH.AREA.g.m2")
EPIP.CALC$EPIP.CARBON.MASS<-EPIP.CALC$EPIP.OM.AREA.g.m2*0.52
EPIP.CALC$EPIP.CHLA.MG.M2.HAUER<-26.7*((EPIP.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-(EPIP.CALC$`Absorbance.at.665.(post.acid)`-EPIP.CALC$`Absorbance.at.750.(post.acid)`))*EPIP.CALC$`VOLUME.OF.ACETONE.USED.(L)`/EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS
EPIP.CALC$EPIP.PHAEO.MG.M2.HAUER<-26.7*((1.7*EPIP.CALC$`Absorbance.at.665.(post.acid)`-EPIP.CALC$`Absorbance.at.750.(post.acid)`)-(EPIP.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*EPIP.CALC$`VOLUME.OF.ACETONE.USED.(L)`/EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS
EPIP.CALC$"EPIP.640/664"<-EPIP.CALC$`Absorbance.at.470.nm.(pre.acid)`/EPIP.CALC$`Absorbance.at.664.nm.(pre.acid)`
EPIP.CALC$"EPIP.PHICOCYANIN.MG/M2"<-(EPIP.CALC$`PHICOCYANIN.(ppb).FINAL`/10^6)*EPIP.CALC$"DILLUTION.FACTOR.PHYCO"*EPIP.CALC$"TOTAL.VOLUME.OF.SAMPLE.(ml)"/(EPIP.CALC$SAMPLER.AREA/10000)
EPIP.CALC$EPIP.CHLA.MG.M2.RITCHIE<-(-0.3319*(EPIP.CALC$`Absorbance.at.630.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.7485*(EPIP.CALC$`Absorbance.at.647.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+11.9442*(EPIP.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.4306*(EPIP.CALC$`Absorbance.at.691.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(EPIP.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
EPIP.CALC$EPIP.CHLB.MG.M2.RITCHIE<-(-1.2825*(EPIP.CALC$`Absorbance.at.630.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+19.8839*(EPIP.CALC$`Absorbance.at.647.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-4.886*(EPIP.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-2.3416*(EPIP.CALC$`Absorbance.at.691.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(EPIP.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
EPIP.CALC$EPIP.CHLC.MG.M2.RITCHIE<-(23.5902*(EPIP.CALC$`Absorbance.at.630.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-7.8516*(EPIP.CALC$`Absorbance.at.647.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.5214*(EPIP.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.7443*(EPIP.CALC$`Absorbance.at.691.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(EPIP.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
EPIP.CALC$EPIP.CHLD.MG.M2.RITCHIE<-(-0.5881*(EPIP.CALC$`Absorbance.at.630.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+0.0902*(EPIP.CALC$`Absorbance.at.647.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-0.1564*(EPIP.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+11.0473*(EPIP.CALC$`Absorbance.at.691.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(EPIP.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
EPIP.CALC$EPIP.CHL.TOTAL.MG.M2.RITCHIE<-(21.3877*(EPIP.CALC$`Absorbance.at.630.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+10.3739*(EPIP.CALC$`Absorbance.at.647.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+5.3805*(EPIP.CALC$`Absorbance.at.664.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+5.5309*(EPIP.CALC$`Absorbance.at.691.nm.(pre.acid)`-EPIP.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(EPIP.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(EPIP.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)


EPIP.RESULT<-data.frame(cbind(EPIP.CALC$"PROJECT",
                              EPIP.CALC$"DATE",
                              EPIP.CALC$"SITE",
                              EPIP.CALC$"SAMPLE",
                              EPIP.CALC$"SUBSTRATE",
                              EPIP.CALC$"AVERAGE.DEPTH",
                              EPIP.CALC$"EPIP.OM.AREA.g.m2",
                              EPIP.CALC$"EPIP.ASH.AREA.g.m2",
                              EPIP.CALC$"EPIP.CHLA.MG.M2.HAUER",
                              EPIP.CALC$"EPIP.PHAEO.MG.M2.HAUER",
                              EPIP.CALC$"EPIP.640/664",
                              EPIP.CALC$"EPIP.PHICOCYANIN.MG/M2",
                              EPIP.CALC$"EPIP.CHLA.MG.M2.RITCHIE",
                              EPIP.CALC$"EPIP.CHLB.MG.M2.RITCHIE",
                              EPIP.CALC$"EPIP.CHLC.MG.M2.RITCHIE",
                              EPIP.CALC$"EPIP.CHLD.MG.M2.RITCHIE",
                              EPIP.CALC$"EPIP.CHL.TOTAL.MG.M2.RITCHIE"))

names(EPIP.RESULT)<-cbind("PROJECT",
                          "DATE",
                          "SITE",
                          "SAMPLE",
                          "SUBSTRATE",
                          "AVERAGE.DEPTH",
                          "EPIP.OM.AREA.g.m2",
                          "EPIP.ASH.AREA.g.m2",
                          "EPIP.CHLA.MG.M2.HAUER",
                          "EPIP.PHAEO.MG.M2.HAUER",
                          "EPIP.640/664",
                          "EPIP.PHICOCYANIN.MG/M2",
                          "EPIP.CHLA.MG.M2.RITCHIE",
                          "EPIP.CHLB.MG.M2.RITCHIE",
                          "EPIP.CHLC.MG.M2.RITCHIE",
                          "EPIP.CHLD.MG.M2.RITCHIE",
                          "EPIP.CHL.TOTAL.MG.M2.RITCHIE")

write.csv(EPIP.RESULT,"2_Incremental/2_Calculation/EPIP.RESULT.csv",
          row.names = FALSE)

rm(list=setdiff(ls(), "path"))

######FBOM CALCULATIONS#######


#LIST OF SPREADSHEETS REQUIRED

#c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON")),

FBOM.LIST.FIELD<-c(list.files(path= path, full.names=TRUE,pattern=c("FIELD")))

FBOM.LIST.AFDM<-c(list.files(path= path, full.names=TRUE,pattern=c("FBOM_AFDM")))

FBOM.LIST.PIGMENTS<-c(list.files(path= path, full.names=TRUE,pattern=c("FBOM_PIGMENTS")))

#TURN ALL SPREADSHEETS INTO A LIST OF OBJECTS

FIELD<-lapply(FBOM.LIST.FIELD,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


AFDM<-lapply(FBOM.LIST.AFDM,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})

PIGMENTS<-lapply(FBOM.LIST.PIGMENTS,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


#MERGE SPREADSHEETS BY DESIRED CATEGORIES

FBOM.FIELD.CALC<-FIELD%>%reduce(full_join)

FBOM.AFDM.CALC<-AFDM%>%reduce(full_join)

file.remove("2_Incremental/1_Digitalization/~$NFAS_FBOM_AFDM_RUNNING_DIG.xlsx")

FBOM.PIGMENTS.CALC<-PIGMENTS%>%reduce(full_join)

All<-list(FBOM.AFDM.CALC,FBOM.PIGMENTS.CALC,FBOM.FIELD.CALC)

FBOM.CALC<-All%>%reduce(full_join, by = c("PROJECT","DATE","SITE","SAMPLE"))


FBOM.CALC$AVERAGE.DEPTH<-(FBOM.CALC$"DEPTH.1.(cm)"+FBOM.CALC$"DEPTH.2.(cm)"+FBOM.CALC$"DEPTH.3.(cm)"+FBOM.CALC$"DEPTH.4.(cm)")/4
FBOM.CALC$SRAD<-16.5
FBOM.CALC$SAMPLER.AREA<-3.14159226*FBOM.CALC$SRAD^2
FBOM.CALC$FIELD.SAMPLE.VOLUME<-FBOM.CALC$"AVERAGE.DEPTH"*FBOM.CALC$"SAMPLER.AREA"
FBOM.CALC$FBOM.DRY.ASH.ON.FILTER<-FBOM.CALC$"FILTERED.AFDM.(g)"-FBOM.CALC$"FILTER.WEIGHT.(g)"
FBOM.CALC$FBOM.OM.ON.FILTER<-FBOM.CALC$"FILTERED.DRY.WEIGHT.(g)"-FBOM.CALC$"FILTERED.AFDM.(g)"
FBOM.CALC$FBOM.BIOMASS.FILTERED.RATIO<-FBOM.CALC$"FILTERED.VOLUME.(mL)"/FBOM.CALC$"SAMPLE.VOLUME.(mL)"
FBOM.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS<-(FBOM.CALC$SAMPLER.AREA)/10000/((FBOM.CALC$AVERAGE.DEPTH/100)*(3.14159226*16.5^2)/10000*1000000/FBOM.CALC$`TOTAL.VOLUME.OF.SAMPLE.(ml)`*FBOM.CALC$`TOTAL.VOLUME.OF.SAMPLE.(ml)`/FBOM.CALC$`VOLUME.OF.SAMPLE.FILTERED.(ml)`)
FBOM.CALC$FBOM.AREA.g.m2<-(FBOM.CALC$"FBOM.OM.ON.FILTER"/FBOM.CALC$"FBOM.BIOMASS.FILTERED.RATIO")*(FBOM.CALC$"FIELD.SAMPLE.VOLUME"/FBOM.CALC$"SAMPLE.VOLUME.(mL)")/FBOM.CALC$SAMPLER.AREA*0.0001
FBOM.CALC$FBOM.ASH.AREA.g.m2<-(FBOM.CALC$FBOM.DRY.ASH.ON.FILTER/FBOM.CALC$"FBOM.BIOMASS.FILTERED.RATIO")*(FBOM.CALC$"FIELD.SAMPLE.VOLUME"/FBOM.CALC$"SAMPLE.VOLUME.(mL)")/FBOM.CALC$SAMPLER.AREA*0.0001
FBOM.CALC$FBOM.ORGANIC.PROPORTION<-FBOM.CALC$"FBOM.AREA.g.m2"/(FBOM.CALC$"FBOM.AREA.g.m2"+FBOM.CALC$"FBOM.ASH.AREA.g.m2")
FBOM.CALC$FBOM.CARBON.MASS<-FBOM.CALC$FBOM.AREA.g.m2*0.52
FBOM.CALC$FBOM.CHLA.MG.M2.HAUER<-26.7*((FBOM.CALC$`Absorbance.at.664.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-(FBOM.CALC$`Absorbance.at.665.(post.acid)`-FBOM.CALC$`Absorbance.at.750.(post.acid)`))*FBOM.CALC$`VOLUME.OF.ACETONE.USED.(L)`/FBOM.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS
FBOM.CALC$FBOM.PHAEO.MG.M2.HAUER<-26.7*((1.7*FBOM.CALC$`Absorbance.at.665.(post.acid)`-FBOM.CALC$`Absorbance.at.750.(post.acid)`)-(FBOM.CALC$`Absorbance.at.664.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*FBOM.CALC$`VOLUME.OF.ACETONE.USED.(L)`/FBOM.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS
FBOM.CALC$"FBOM.470/664"<-FBOM.CALC$`Absorbance.at.470.nm.(pre.acid)`/FBOM.CALC$`Absorbance.at.664.nm.(pre.acid)`
FBOM.CALC$`FBOM.PHICOCYANIN.MG/M2`<-(FBOM.CALC$`PHICOCYANIN.(ppb)`/10^6)*FBOM.CALC$FIELD.SAMPLE.VOLUME/(FBOM.CALC$SAMPLER.AREA/10000)
FBOM.CALC$FBOM.CHLA.MG.M2.RITCHIE<-(-0.3319*(FBOM.CALC$`Absorbance.at.630.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.7485*(FBOM.CALC$`Absorbance.at.647.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+11.9442*(FBOM.CALC$`Absorbance.at.664.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.4306*(FBOM.CALC$`Absorbance.at.691.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(FBOM.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(FBOM.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
FBOM.CALC$FBOM.CHLB.MG.M2.RITCHIE<-(-1.2825*(FBOM.CALC$`Absorbance.at.630.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+19.8839*(FBOM.CALC$`Absorbance.at.647.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-4.886*(FBOM.CALC$`Absorbance.at.664.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-2.3416*(FBOM.CALC$`Absorbance.at.691.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(FBOM.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(FBOM.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
FBOM.CALC$FBOM.CHLC.MG.M2.RITCHIE<-(23.5902*(FBOM.CALC$`Absorbance.at.630.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-7.8516*(FBOM.CALC$`Absorbance.at.647.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.5214*(FBOM.CALC$`Absorbance.at.664.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.7443*(FBOM.CALC$`Absorbance.at.691.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(FBOM.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(FBOM.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
FBOM.CALC$FBOM.CHLD.MG.M2.RITCHIE<-(-0.5881*(FBOM.CALC$`Absorbance.at.630.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+0.0902*(FBOM.CALC$`Absorbance.at.647.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-0.1564*(FBOM.CALC$`Absorbance.at.664.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+11.0473*(FBOM.CALC$`Absorbance.at.691.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(FBOM.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(FBOM.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)
FBOM.CALC$FBOM.CHL.TOTAL.MG.M2.RITCHIE<-(21.3877*(FBOM.CALC$`Absorbance.at.630.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+10.3739*(FBOM.CALC$`Absorbance.at.647.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+5.3805*(FBOM.CALC$`Absorbance.at.664.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+5.5309*(FBOM.CALC$`Absorbance.at.691.nm.(pre.acid)`-FBOM.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(FBOM.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(FBOM.CALC$AREA.EQUIVALENT.ON.FILTER.PIGMENTS)


FBOM.RESULT<-data.frame(cbind(FBOM.CALC$"PROJECT",
                              FBOM.CALC$"DATE",
                              FBOM.CALC$"SITE",
                              FBOM.CALC$"SAMPLE",
                              FBOM.CALC$"SUBSTRATE",
                              FBOM.CALC$"AVERAGE.DEPTH",
                              FBOM.CALC$"FBOM.AREA.g.m2",
                              FBOM.CALC$"FBOM.ASH.AREA.g.m2",
                              FBOM.CALC$"FBOM.CHLA.MG.M2.HAUER",
                              FBOM.CALC$"FBOM.PHAEO.MG.M2.HAUER",
                              FBOM.CALC$"FBOM.470/664",
                              FBOM.CALC$"FBOM.PHICOCYANIN.MG/M2",
                              FBOM.CALC$"FBOM.CHLA.MG.M2.RITCHIE",
                              FBOM.CALC$"FBOM.CHLB.MG.M2.RITCHIE",
                              FBOM.CALC$"FBOM.CHLC.MG.M2.RITCHIE",
                              FBOM.CALC$"FBOM.CHLD.MG.M2.RITCHIE",
                              FBOM.CALC$"FBOM.CHL.TOTAL.MG.M2.RITCHIE"))

names(FBOM.RESULT)<-cbind("PROJECT",
                          "DATE",
                          "SITE",
                          "SAMPLE",
                          "SUBSTRATE",
                          "AVERAGE.DEPTH", 
                          "FBOM.AREA.g.m2",
                          "FBOM.ASH.AREA.g.m2",
                          "FBOM.CHLA.MG.M2.HAUER",
                          "FBOM.PHAEO.MG.M2.HAUER",
                          "FBOM.640/664",
                          "FBOM.PHICOCYANIN.MG/M2",
                          "FBOM.CHLA.MG.M2.RITCHIE",
                          "FBOM.CHLB.MG.M2.RITCHIE",
                          "FBOM.CHLC.MG.M2.RITCHIE",
                          "FBOM.CHLD.MG.M2.RITCHIE",
                          "FBOM.CHL.TOTAL.MG.M2.RITCHIE")

write.csv(FBOM.RESULT,"2_Incremental/2_Calculation/FBOM.RESULT.csv",
          row.names = FALSE)

rm(list=setdiff(ls(), "path"))

######FILAMENTOUS CALCULATIONS#######


#LIST OF SPREADSHEETS REQUIRED

#c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON")),

FILA.LIST.FIELD<-c(list.files(path= path, full.names=TRUE,pattern=c("FIELD")))

FILA.LIST.AFDM<-c(list.files(path= path, full.names=TRUE,pattern=c("FILAMENTOUS_AFDM")))

FILA.LIST.PIGMENTS<-c(list.files(path= path, full.names=TRUE,pattern=c("FILAMENTOUS_PIGMENTS")))

#TURN ALL SPREADSHEETS INTO A LIST OF OBJECTS

FIELD<-lapply(FILA.LIST.FIELD,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


AFDM<-lapply(FILA.LIST.AFDM,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})

PIGMENTS<-lapply(FILA.LIST.PIGMENTS,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


#MERGE SPREADSHEETS BY DESIRED CATEGORIES

FILA.FIELD.CALC<-FIELD%>%reduce(full_join)

FILA.AFDM.CALC<-AFDM%>%reduce(full_join)

FILA.PIGMENTS.CALC<-PIGMENTS%>%reduce(full_join)

All<-list(FILA.AFDM.CALC,FILA.PIGMENTS.CALC,FILA.FIELD.CALC)

FILA.CALC<-All%>%reduce(full_join, by = c("PROJECT","DATE","SITE","SAMPLE"))

names(FILA.CALC)


FILA.CALC$AVERAGE.DEPTH<-(FILA.CALC$"DEPTH.1.(cm)"+FILA.CALC$"DEPTH.2.(cm)"+FILA.CALC$"DEPTH.3.(cm)"+FILA.CALC$"DEPTH.4.(cm)")/4
FILA.CALC$SRAD<-16.5
FILA.CALC$SAMPLER.AREA<-3.14159226*FILA.CALC$SRAD^2

FILA.CALC$"FILA.OM.AREA.g.m2"<-
  ifelse(is.na(FILA.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)")
         , (FILA.CALC$"SUBSAMPLE+TRAY.DRY.WEIGHT.(g)"-FILA.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)")/FILA.CALC$SAMPLER.AREA*10000
         , (FILA.CALC$"SUBSAMPLE+TRAY.DRY.WEIGHT.(g)"-FILA.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)")*((FILA.CALC$"SAMPLE+TRAY.WET.WEIGHT.(g)"-FILA.CALC$"SAMPLE.TRAY.WEIGHT.(g)")/(FILA.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)"-FILA.CALC$"SUBSAMPLE.TRAY.WEIGHT"))/FILA.CALC$SAMPLER.AREA*10000 
  )
  
FILA.CALC$"FILA.ASH.AREA.g.m2"<-ifelse(is.na(FILA.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)")
                                       , (FILA.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)"-FILA.CALC$"SAMPLE.TRAY.WEIGHT.(g)")/FILA.CALC$SAMPLER.AREA*10000
                                       , (FILA.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)"-FILA.CALC$"SUBSAMPLE.TRAY.WEIGHT")*((FILA.CALC$"SAMPLE+TRAY.WET.WEIGHT.(g)"-FILA.CALC$"SAMPLE.TRAY.WEIGHT.(g)")/(FILA.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)"-FILA.CALC$"SUBSAMPLE.TRAY.WEIGHT"))/FILA.CALC$SAMPLER.AREA*10000 
)
FILA.CALC$AREA.EQUIVALENT.ON.MASS.EXTRACT.PIGMENTS<-FILA.CALC$"WEIGHT.OF.SUBSAMPLE.EXTRACTED.(g)"/(FILA.CALC$"SAMPLE+TRAY.WET.WEIGHT.(g)"-FILA.CALC$"SAMPLE.TRAY.WEIGHT.(g)")*FILA.CALC$SAMPLER.AREA/10000
FILA.CALC$FILA.CHLA.MG.M2.HAUER<-26.7*((FILA.CALC$`Absorbance.at.664.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-(FILA.CALC$`Absorbance.at.665.(post.acid)`-FILA.CALC$`Absorbance.at.750.(post.acid)`))*FILA.CALC$`VOLUME.OF.ACETONE.USED.(L)`/FILA.CALC$AREA.EQUIVALENT.ON.MASS.EXTRACT.PIGMENTS
FILA.CALC$FILA.PHAEO.MG.M2.HAUER<-26.7*((1.7*FILA.CALC$`Absorbance.at.665.(post.acid)`-FILA.CALC$`Absorbance.at.750.(post.acid)`)-(FILA.CALC$`Absorbance.at.664.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*FILA.CALC$`VOLUME.OF.ACETONE.USED.(L)`/FILA.CALC$AREA.EQUIVALENT.ON.MASS.EXTRACT.PIGMENTS
FILA.CALC$"FILA.470/664"<-FILA.CALC$`Absorbance.at.470.nm.(pre.acid)`/FILA.CALC$`Absorbance.at.664.nm.(pre.acid)`
FILA.CALC$FILA.CHLA.MG.M2.RITCHIE<-(-0.3319*(FILA.CALC$`Absorbance.at.630.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.7485*(FILA.CALC$`Absorbance.at.647.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+11.9442*(FILA.CALC$`Absorbance.at.664.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.4306*(FILA.CALC$`Absorbance.at.691.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(FILA.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/FILA.CALC$AREA.EQUIVALENT.ON.MASS.EXTRACT.PIGMENTS
FILA.CALC$FILA.CHLB.MG.M2.RITCHIE<-(-1.2825*(FILA.CALC$`Absorbance.at.630.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+19.8839*(FILA.CALC$`Absorbance.at.647.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-4.886*(FILA.CALC$`Absorbance.at.664.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-2.3416*(FILA.CALC$`Absorbance.at.691.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(FILA.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/FILA.CALC$AREA.EQUIVALENT.ON.MASS.EXTRACT.PIGMENTS
FILA.CALC$FILA.CHLC.MG.M2.RITCHIE<-(23.5902*(FILA.CALC$`Absorbance.at.630.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-7.8516*(FILA.CALC$`Absorbance.at.647.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.5214*(FILA.CALC$`Absorbance.at.664.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-1.7443*(FILA.CALC$`Absorbance.at.691.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(FILA.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/FILA.CALC$AREA.EQUIVALENT.ON.MASS.EXTRACT.PIGMENTS
FILA.CALC$FILA.CHLD.MG.M2.RITCHIE<-(-0.5881*(FILA.CALC$`Absorbance.at.630.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+0.0902*(FILA.CALC$`Absorbance.at.647.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)-0.1564*(FILA.CALC$`Absorbance.at.664.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+11.0473*(FILA.CALC$`Absorbance.at.691.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(FILA.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/FILA.CALC$AREA.EQUIVALENT.ON.MASS.EXTRACT.PIGMENTS
FILA.CALC$FILA.CHL.TOTAL.MG.M2.RITCHIE<-(21.3877*(FILA.CALC$`Absorbance.at.630.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+10.3739*(FILA.CALC$`Absorbance.at.647.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+5.3805*(FILA.CALC$`Absorbance.at.664.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`)+5.5309*(FILA.CALC$`Absorbance.at.691.nm.(pre.acid)`-FILA.CALC$`Absorbance.at.750.nm.(pre.Acid)`))*(FILA.CALC$`VOLUME.OF.ACETONE.USED.(L)`)/(FILA.CALC$AREA.EQUIVALENT.ON.MASS.EXTRACT.PIGMENTS)

FILA.RESULT<-data.frame(cbind(FILA.CALC$"PROJECT",
                              FILA.CALC$"DATE",
                              FILA.CALC$"SITE",
                              FILA.CALC$"SAMPLE",
                              FILA.CALC$"SUBSTRATE",
                              FILA.CALC$"AVERAGE.DEPTH",
                              FILA.CALC$"FILA.OM.AREA.g.m2",
                              FILA.CALC$"FILA.ASH.AREA.g.m2",
                              FILA.CALC$"FILA.CHLA.MG.M2.HAUER",
                              FILA.CALC$"FILA.PHAEO.MG.M2.HAUER",
                              FILA.CALC$"FILA.470/664",
                              FILA.CALC$"FILA.CHLA.MG.M2.RITCHIE",
                              FILA.CALC$"FILA.CHLB.MG.M2.RITCHIE",
                              FILA.CALC$"FILA.CHLC.MG.M2.RITCHIE",
                              FILA.CALC$"FILA.CHLD.MG.M2.RITCHIE",
                              FILA.CALC$"FILA.CHL.TOTAL.MG.M2.RITCHIE"))

names(FILA.RESULT)<-cbind("PROJECT",
                          "DATE",
                          "SITE",
                          "SAMPLE",
                          "SUBSTRATE",
                          "AVERAGE.DEPTH",
                          "FILA.OM.AREA.g.m2",
                          "FILA.ASH.AREA.g.m2",
                          "FILA.CHLA.MG.M2.HAUER",
                          "FILA.PHAEO.MG.M2.HAUER",
                          "FILA .640/664",
                          "FILA.CHLA.MG.M2.RITCHIE",
                          "FILA.CHLB.MG.M2.RITCHIE",
                          "FILA.CHLC.MG.M2.RITCHIE",
                          "FILA.CHLD.MG.M2.RITCHIE",
                          "FILA.CHL.TOTAL.MG.M2.RITCHIE")

write.csv(FILA.RESULT,"2_Incremental/2_Calculation/FILA.RESULT.csv",
          row.names = FALSE)

rm(list=setdiff(ls(), "path"))

######CPOM CALCULATIONS#######

#LIST OF SPREADSHEETS REQUIRED

#c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON")),

CPOM.LIST.FIELD<-c(list.files(path= path, full.names=TRUE,pattern=c("FIELD")))

CPOM.LIST.AFDM<-c(list.files(path= path, full.names=TRUE,pattern=c("CPOM_AFDM")))

#TURN ALL SPREADSHEETS INTO A LIST OF OBJECTS

FIELD<-lapply(CPOM.LIST.FIELD,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


AFDM<-lapply(CPOM.LIST.AFDM,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


#MERGE SPREADSHEETS BY DESIRED CATEGORIES

CPOM.FIELD.CALC<-FIELD%>%reduce(full_join)

CPOM.AFDM.CALC<-AFDM%>%reduce(full_join)

All<-list(CPOM.AFDM.CALC,CPOM.FIELD.CALC)

CPOM.CALC<-All%>%reduce(full_join, by = c("PROJECT","DATE","SITE","SAMPLE","SUBSTRATE"))


CPOM.CALC$AVERAGE.DEPTH<-(CPOM.CALC$"DEPTH.1.(cm)"+CPOM.CALC$"DEPTH.2.(cm)"+CPOM.CALC$"DEPTH.3.(cm)"+CPOM.CALC$"DEPTH.4.(cm)")/4
CPOM.CALC$SRAD<-16.5
CPOM.CALC$SAMPLER.AREA<-3.14159226*CPOM.CALC$SRAD^2

CPOM.CALC$"CPOM.OM.AREA.g.m2"<-
  ifelse(is.na(CPOM.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)")
         , (CPOM.CALC$"SUBSAMPLE+TRAY.DRY.WEIGHT.(g)"-CPOM.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)")/CPOM.CALC$SAMPLER.AREA*10000
         , (CPOM.CALC$"SUBSAMPLE+TRAY.DRY.WEIGHT.(g)"-CPOM.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)")*((CPOM.CALC$"SAMPLE+TRAY.WET.WEIGHT.(g)"-CPOM.CALC$"SAMPLE.TRAY.WEIGHT.(g)")/(CPOM.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)"-CPOM.CALC$"SUBSAMPLE.TRAY.WEIGHT"))/CPOM.CALC$SAMPLER.AREA*10000 
  )

CPOM.CALC$"CPOM.ASH.AREA.g.m2"<-ifelse(is.na(CPOM.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)")
                                       , (CPOM.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)"-CPOM.CALC$"SAMPLE.TRAY.WEIGHT.(g)")/CPOM.CALC$SAMPLER.AREA*10000
                                       , (CPOM.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)"-CPOM.CALC$"SUBSAMPLE.TRAY.WEIGHT")*((CPOM.CALC$"SAMPLE+TRAY.WET.WEIGHT.(g)"-CPOM.CALC$"SAMPLE.TRAY.WEIGHT.(g)")/(CPOM.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)"-CPOM.CALC$"SUBSAMPLE.TRAY.WEIGHT"))/CPOM.CALC$SAMPLER.AREA*10000 
)
CPOM.RESULT<-data.frame(cbind(CPOM.CALC$"PROJECT",
                              CPOM.CALC$"DATE",
                              CPOM.CALC$"SITE",
                              CPOM.CALC$"SAMPLE",
                              CPOM.CALC$"SUBSTRATE",
                              CPOM.CALC$"AVERAGE.DEPTH",
                              CPOM.CALC$"CPOM.OM.AREA.g.m2",
                              CPOM.CALC$"CPOM.ASH.AREA.g.m2"))

names(CPOM.RESULT)<-cbind("PROJECT",
                          "DATE",
                          "SITE",
                          "SAMPLE",
                          "SUBSTRATE",
                          "AVERAGE.DEPTH",
                          "CPOM.OM.AREA.g.m2",
                          "CPOM.ASH.AREA.g.m2")

write.csv(CPOM.RESULT,"2_Incremental/2_Calculation/CPOM.RESULT.csv",
          row.names = FALSE)

rm(list=setdiff(ls(), "path"))

######MACROPHYTES CALCULATIONS#######


#LIST OF SPREADSHEETS REQUIRED

#c(list.files(path= path, full.names=TRUE,pattern=c("EPILITHON")),

MACRO.LIST.FIELD<-c(list.files(path= path, full.names=TRUE,pattern=c("FIELD")))

MACRO.LIST.AFDM<-c(list.files(path= path, full.names=TRUE,pattern=c("MACRO_AFDM")))

#TURN ALL SPREADSHEETS INTO A LIST OF OBJECTS

FIELD<-lapply(MACRO.LIST.FIELD,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})


AFDM<-lapply(MACRO.LIST.AFDM,function(filename){
  print(paste("Merging",filename,sep.=""))
  read.xlsx(filename)
})

#MERGE SPREADSHEETS BY DESIRED CATEGORIES

MACRO.FIELD.CALC<-FIELD%>%reduce(full_join)

MACRO.AFDM.CALC<-AFDM%>%reduce(full_join)

All<-list(MACRO.AFDM.CALC,MACRO.FIELD.CALC)

MACRO.CALC<-All%>%reduce(full_join, by = c("PROJECT","DATE","SITE","SAMPLE","SUBSTRATE"))


MACRO.CALC$AVERAGE.DEPTH<-(MACRO.CALC$"DEPTH.1.(cm)"+MACRO.CALC$"DEPTH.2.(cm)"+MACRO.CALC$"DEPTH.3.(cm)"+MACRO.CALC$"DEPTH.4.(cm)")/4
MACRO.CALC$SRAD<-16.5
MACRO.CALC$SAMPLER.AREA<-3.14159226*MACRO.CALC$SRAD^2

MACRO.CALC$"MACRO.OM.AREA.g.m2"<-
  ifelse(is.na(MACRO.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)")
         , (MACRO.CALC$"SUBSAMPLE+TRAY.DRY.WEIGHT.(g)"-MACRO.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)")/MACRO.CALC$SAMPLER.AREA*10000
         , (MACRO.CALC$"SUBSAMPLE+TRAY.DRY.WEIGHT.(g)"-MACRO.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)")*((MACRO.CALC$"SAMPLE+TRAY.WET.WEIGHT.(g)"-MACRO.CALC$"SAMPLE.TRAY.WEIGHT.(g)")/(MACRO.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)"-MACRO.CALC$"SUBSAMPLE.TRAY.WEIGHT"))/MACRO.CALC$SAMPLER.AREA*10000 
  )

MACRO.CALC$"MACRO.ASH.AREA.g.m2"<-ifelse(is.na(MACRO.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)")
                                       , (MACRO.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)"-MACRO.CALC$"SAMPLE.TRAY.WEIGHT.(g)")/MACRO.CALC$SAMPLER.AREA*10000
                                       , (MACRO.CALC$"SUBSAMPLE.+TRAY.ASHED.WEIGHT(g)"-MACRO.CALC$"SUBSAMPLE.TRAY.WEIGHT")*((MACRO.CALC$"SAMPLE+TRAY.WET.WEIGHT.(g)"-MACRO.CALC$"SAMPLE.TRAY.WEIGHT.(g)")/(MACRO.CALC$"SUBSAMPLE+TRAY.WET.WEIGHT.(g)"-MACRO.CALC$"SUBSAMPLE.TRAY.WEIGHT"))/MACRO.CALC$SAMPLER.AREA*10000 
)

MACRO.RESULT<-data.frame(cbind(MACRO.CALC$"PROJECT",
                               MACRO.CALC$"DATE",
                               MACRO.CALC$"SITE",
                               MACRO.CALC$"SAMPLE",
                               MACRO.CALC$"SUBSTRATE",
                               MACRO.CALC$"AVERAGE.DEPTH",
                               MACRO.CALC$"MACRO.OM.AREA.g.m2",
                               MACRO.CALC$"MACRO.ASH.AREA.g.m2"))

names(MACRO.RESULT)<-cbind("PROJECT",
                           "DATE",
                           "SITE",
                           "SAMPLE",
                           "SUBSTRATE",
                           "AVERAGE.DEPTH",
                           "MACRO.OM.AREA.g.m2",
                           "MACRO.ASH.AREA.g.m2")

write.csv(MACRO.RESULT,"2_Incremental/2_Calculation/MACRO.RESULT.csv",
          row.names = FALSE)

rm(list = ls())

