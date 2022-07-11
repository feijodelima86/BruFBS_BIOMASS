library(readr)
library(plyr)

alldata <- data.frame(read_csv("2_Incremental/3_Integration/INTEGRATED.csv"))

alldata$DATE<-as.Date(alldata$DATE,"%m/%d/%Y")

# Variable date to factor

datefactor <- factor(alldata[,3])

se <- function(x, ...) sqrt(var(x, ...)/length(x))

ssite<-"DL"

names(alldata)


n1<-20
n2<-55

a<-data.frame(aggregate(alldata[,n1] ~ DATE+SITE, alldata, mean))
a[,4]<-aggregate(alldata[,n1] ~ DATE+SITE, alldata, se)[,3]
a$DATE > as.Date(as.character("2021-01-01"))
a <- subset(a, DATE < as.Date(as.character("2021-01-01")))
a <- subset(a, SITE == ssite)
names(a)<-c("DATE","SITE","MEAN","STDER")
a[,5]<--2


a2<-data.frame(aggregate(alldata[,n2] ~ DATE+SITE, alldata, mean))
a2[,4]<-aggregate(alldata[,n2] ~ DATE+SITE, alldata, se)[,3]
a2$DATE > as.Date(as.character("2021-01-01"))
a2 <- subset(a2, DATE < as.Date(as.character("2021-01-01")))
a2 <- subset(a2, SITE == ssite)
names(a2)<-c("DATE","SITE","MEAN","STDER")
a2[,5]<--2

n=length(a[,2])

j=2

#each 'polygon' is inside a list with xx and yy coordinates

dat1 <- lapply(1:n,function(x){
  res <- list(xx=c(a[x,1]+j, a[x,1]-j, a[x,1]-j, a[x,1]+j),
              yy=c(a[x,5], a[x,5], a[x,3], a[x,3]))
  return(res)
})

dat2 <- lapply(1:n,function(x){
  res <- list(xx=c(a2[x,1]+j, a2[x,1]-j, a2[x,1]-j, a2[x,1]+j),
              yy=c(a2[x,5], a2[x,5], a2[x,3], a2[x,3]))
  return(res)
})



#create empty plot

dev.new(width=10, height=7, noRStudioGD = TRUE)
plot(a[,1], a[,2],
     type='n',
     ylim=c(10,300),
     xlim= c(as.Date("2020-06-20"),as.Date("2020-10-30")),
     ylab=NA, 
     yaxt = "n",
     cex=1.5,
     xaxt = "n",
     xlab = NA,
     cex.lab=1, cex.axis=1.2, cex.main=1.2, cex.sub=1.2,
)

#title(ylab=Ylabel, line=2, cex.lab=1.2, family="Calibri Light")


range(a2[,2])

axis(side = 2, at = c(0, 100, 200, 300, 400, 500), las=1, font.axis=1, cex.axis=1.75)

#add polygons

for (i in 1:n) polygon(unlist(dat2[[i]]$xx),unlist(dat2[[i]]$yy),col="chartreuse3", border="black")
for (i in 1:n) polygon(unlist(dat1[[i]]$xx),unlist(dat1[[i]]$yy),col=colors()[89],border="black")


box(lwd=4)
axis.Date(1, at=seq(as.Date("2020-06-20"), as.Date("2020-10-30"), "1 week"), las=1, font.axis=2, cex.axis=1.5, format="%d-%b")
arrows(a[,1], a[,3], a[,1], a[,3]+a[,4], length=0.075, angle=90, code=2, lwd=2,col="black")
arrows(a2[,1], a2[,3], a2[,1], a2[,3]+a2[,4], length=0.075, angle=90, code=2, lwd=2,col="black")
