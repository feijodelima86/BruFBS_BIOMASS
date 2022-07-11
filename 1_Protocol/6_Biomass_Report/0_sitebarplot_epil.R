library(readr)
library(plyr)

alldata <- data.frame(read_csv("2_Incremental/3_Integration/INTEGRATED_NATOO.csv"))

alldata$DATE<-as.Date(alldata$DATE,"%m/%d/%Y")

# Variable date to factor
datefactor <- factor(alldata$DATE)

se <- function(x, ...) sqrt(var(x, ...)/length(x))

names(alldata)

n1<-20

a<-data.frame(aggregate(alldata[,n1] ~ DATE+SITE, alldata, mean))
a[,4]<-aggregate(alldata[,n1] ~ DATE+SITE, alldata, se)[,3]
a

d.a <- subset(a, DATE > as.Date("2021-01-01"))

names(d.a)<-c("DATE","SITE","MEAN","STDER")

d.a$SITENUM<-c(10,11,6,9,8,1)

d.a<-d.a[order(d.a$SITENUM),]


barplot

# create positions for tick marks, one more than number of bars
x <- d.a$MEAN

# create labels
x.labels <- d.a$SITE

# specify colors for groups


barCenters<-barplot(height=x, 
                    space = 1,  
                    col = "darkgreen",
                    names.arg = x.labels,
                    ylab="", 
                    ylim = c(0, max(d.a$MEAN)+max(d.a$STDER)+5),
                    cex=1.5,
                    las=1,
                    cex.lab=1, 
                    cex.axis=1.2, 
                    cex.main=1.2, 
                    cex.sub=1.2,
)


box(lwd=3)


arrows(barCenters, d.a$MEAN , barCenters, d.a$MEAN + d.a$STDER,  
       length=0.2, angle=90, code=2, lwd=3,col="black")
