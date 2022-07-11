library(readr)
library(plyr)


alldata <- data.frame(read_csv("2_Incremental/3_Integration/INTEGRATED.csv"))

alldata$DATE<-as.Date(alldata$DATE,"%m/%d/%Y")

# Variable date to factor
datefactor <- factor(alldata[,1])

se <- function(x, ...) sqrt(var(x, ...)/length(x))

Ylabel=expression(bold(paste("Concentration (", mu, "g/l)")))

names(alldata)

n1<-15


a<-data.frame(aggregate(alldata[,n1] ~ DATE+SITE, alldata, mean))
a[,4]<-aggregate(alldata[,n1] ~ DATE+SITE, alldata, se)[,3]

a$DATE > as.Date(as.character("2021-01-01"))

a <- subset(a, DATE < as.Date(as.character("2021-01-01")))

a <- subset(a, SITE == "BN")

names(a)<-c("DATE","SITE","MEAN","STDER")

a[,5]<--2
d=1
n=length(a[,2])

j=2

#each 'polygon' is inside a list with xx and yy coordinates

dat1 <- lapply(1:n,function(x){
  res <- list(xx=c(a[x,1]+j, a[x,1]-j, a[x,1]-j, a[x,1]+j),
              yy=c(a[x,5], a[x,5], a[x,3], a[x,3]))
  return(res)
})


dev.new(width=10, height=7, noRStudioGD = TRUE)
par(mar=c(4,4,4,4))
par(mgp=c(3,1,0))
plot(a[,1], a[,2],
     type='n',
     ylim=c(0,90),
     xlim= c(as.Date("2020-06-20"),as.Date("2020-10-30")),
     cex=2,
     xaxt = "n",
     xlab = NA,
     yaxt = "n",
     ylab = NA,
     las=1
)


#title(ylab=Ylabel, line=2, cex.lab=1.2, family="Calibri Light", mgp=c(9,9,9))

range(a[,2])

#add polygons

for (i in 1:n) polygon(unlist(dat1[[i]]$xx),unlist(dat1[[i]]$yy),col="deepskyblue4",border="black")

axis.Date(1, at=seq(as.Date("2020-06-20"), as.Date("2020-10-30"), "1 week"), las=1, font.axis=2, cex.axis=1.5, format="%d-%b")
axis(side = 2, at = c(0, 15, 30, 45, 60, 75, 90), las=1, font.axis=1, cex.axis=1.75)

#legend("topright", legend=c("Filamentous", "Epilithic"), fill=c("chartreuse3", colors()[89]),
#       density=c(NA,NA), bty="n",border=c("black", "black")) 

box(lwd=4)

arrows(a[,1]-0, a[,3], a[,1], a[,3]+a[,4], length=0.1, angle=90, code=2, lwd=2,col="black")
#arrows(a2[,1]+0.5, a2[,2], a2[,1]+0.5, a2[,2]+a2[,3], length=0.045, angle=90, code=2, lwd=2 ,col="black")




