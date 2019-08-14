network <- "C6"
dat <- read.csv("Output_specieswithmoreprey_secext_variables_real_6035_C6.dat")
names(dat)
dat <- read.csv("Output_specieswithmoreprey_secext_variables_real_6035_C6.dat",sep=",")
names(dat)
dat <- read.csv("Output_specieswithmoreprey_secext_variables_real_6035_C6.dat",sep="\t")
names(dat)
dat <- read.csv("Output_specieswithmoreprey_secext_variables_real_6035_C6.dat",sep=" ")
names(dat)
dat <- read.csv("Output_specieswithmoreprey_secext_variables_real_6035_C6.dat",sep=";")
names(dat)
dat$PrimExt
dat <- read.csv("Output_specieswithmoreprey_secext_variables_real_6025_C6.dat",sep=";")
dat <- read.csv("Output_specieswithmoreprey_secext_variables_real_6015_C6.dat",sep=";")
dat$PrimExt
files <- system("ls Output_species*C6*.dat")
files <- system("ls Output_species*C6*.dat",intern=TRUE)
files
df <- data.frame()
for(i in 1:length(files)){
if(i == 1){df <- read.csv(files[i],sep=";")}
else{df <- rbind(df,read.csv(files[i],sep=";")}
for(i in 1:length(files)){
if(i == 1){df <- read.csv(files[i],sep=";")}
else{df <- rbind(df,read.csv(files[i],sep=";"))}
}
nrow(files)
length(files)
files
df <- read.csv(files[1],sep=";")
names(df)
files[1]
files <- system("ls Output_species*variables*C6*.dat",intern=TRUE)
files
df <- read.csv(files[1],sep=";")
names(df)
df <- read.csv(files[2],sep=";")
names(df)
df <- data.frame()
for(i in 1:length(files)){
if(i == 1){df <- read.csv(files[i],sep=";")}else{df <- rbind(df,read.csv(files[i],sep=";"))}}
else{df <- rbind(df,read.csv(files[i],sep=";"))}
}
library(lattice)
library(gridBase)
library(MASS)
library(RColorBrewer)
GetColorHexAndDecimal<-function(color){
  c<-col2rgb(color)
  sprintf("#%02X%02X%02X", c[1],c[2],c[3])
}
addalpha<-function(colors, alpha=1.0){
  r<-col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,]<-alpha*255
  r<-r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}
plotShadow<-function(meanvalues,sdvalues,myalpha=0.5,mylwd=2,myxlab="Time",myylab="Fluorecence",mymain="Comparison",filename="shadowcurve.png"){
  #reading the number of points in X-axis
  npoints<-length(meanvalues[1,]);
  #reading the number of lines to be plotted
  ndatasets<-length(meanvalues[,1]);
  #reading the Y-range
  maxYvalue<-max(meanvalues+sdvalues);
  minYvalue<-min(meanvalues-sdvalues);
  minXvalue<-1;
  maxXvalue<-ndatasets;
  #creating the colors for the lines and for the shadows
  line_cols<-sapply(1:ndatasets,GetColorHexAndDecimal)
  shade_cols<-as.character(lapply(line_cols,addalpha,alpha=myalpha));
  png(filename,width=1980,height=1280,res=300);
  plot(c(minXvalue,maxXvalue),c(minYvalue,maxYvalue),type="n",
       ylim=c(minYvalue,maxYvalue),xlim=c(minXvalue,maxXvalue),
       xlab=myxlab,ylab=myylab,pch=3,cex=1.5, bty = "n",
       main = mymain);
  for(i in 1:ndatasets){
    #reading the mean and sd referent to each dataset 'i'
    fit<-meanvalues[i,];
    sd<-sdvalues[i,];
    #defining the covariates (x values)
    covariates<-minXvalue:maxXvalue;
    i.for <- order( covariates )
    i.back <- order( covariates,decreasing=TRUE )
    x.polygon <- c( covariates[i.for] , covariates[i.back] );
    #lower limit of the shadow
    lcl <- fit - sd;
    #upper limit of the shadow
    ucl <- fit + sd;
    y.polygon <- c( ucl[i.for] , lcl[i.back] )
    polygon( x.polygon , y.polygon , col = shade_cols[i] , border = NA ,cex=1.5)
    lines( covariates[i.for], fit[i.for], col = line_cols[i] , lwd = mylwd ,cex=1.5)
#    axis(3,labels=FALSE);
    axis(4,labels=FALSE,cex=1.5);
  }
  dev.off();
}
library(tidyverse)
files <- system("ls Output_species*variables*B3*.dat",intern=TRUE)
df2<-data.frame();for(i in 1:length(files)){if(i == 1){df2 <- read.csv(files[i],sep=";")}else{df2 <- rbind(df2,read.csv(files[i],sep=";"))}}
sample <- rep("B3",nrow(df2))
df2$sample <- sample
sample <- rep("C6",nrow(df))
df$sample <- sample
nrow(df)
nrow(df2)
names(df)
df %>% group_by(PrimExt) %>% summarise(meanSE = mean(CumSecExt)) %>% ggplot(aes(x = PrimExt, y = meanSE)) + geom_line(stat="identity")
df3 <- rbind(df,df2)
df3 %>% group_by(PrimExt, sample) %>% summarise(meanSE = mean(CumSecExt)) %>% ggplot(aes(x = PrimExt, y = meanSE, col=sample)) + geom_line(stat="identity")
max(df$PrimExt)
max(df$PrimExt)+max(df$CumSecExt)
rep(max(df$PrimExt)+max(df$CumSecExt),nrow(df))
df$nvert <- rep(max(df$PrimExt)+max(df$CumSecExt),nrow(df))
df2$nvert <- rep(max(df2$PrimExt)+max(df2$CumSecExt),nrow(df2))
df3 <- rbind(df,df2)
df3 %>% group_by(PrimExt, sample,nvert) %>% summarise(meanSE = mean(CumSecExt)) %>% ggplot(aes(x = 100*PrimExt/nvert, y = 100*meanSE/nvert, col=sample)) + geom_line(stat="identity")
df3 %>% group_by(PrimExt, sample,nvert) %>% summarise(meanSE = mean(CumSecExt/nvert)) %>% ggplot(aes(x = 100*PrimExt/nvert, y = meanSE, col=sample)) + geom_line(stat="identity")
df3 %>% group_by(PrimExt, sample,nvert) %>% summarise(meanSE = mean(CumSecExt/nvert)) %>% ggplot(aes(x = 100*PrimExt/nvert, y = 100*meanSE, col=sample)) + geom_line(stat="identity")
df3 %>% group_by(PrimExt, sample,nvert) %>% summarise(meanSE = mean(CumSecExt/nvert)) %>% ggplot(aes(x = 100*PrimExt/nvert, y = 100*meanSE, col=sample)) + geom_line(stat="identity") + xlim(0,100)
df3 %>% group_by(PrimExt, sample,nvert) %>% summarise(meanSE = mean(CumSecExt/nvert)) %>% ggplot(aes(x = 100*PrimExt/nvert, y = 100*meanSE, col=sample)) + geom_line(stat="identity") + xlim(0,100)+ylim(0,30)
df3 %>% group_by(PrimExt, sample,nvert) %>% summarise(meanSE = mean(CumSecExt/nvert)) %>% ggplot(aes(x = 100*PrimExt/nvert, y = 100*meanSE, col=sample)) + geom_line(stat="identity") + xlim(0,100)+ylim(0,30)+labs(title="Robustness - Secondary Extinction")
df3 %>% group_by(PrimExt, sample,nvert) %>% summarise(meanSE = mean(CumSecExt/nvert)) %>% ggplot(aes(x = 100*PrimExt/nvert, y = 100*meanSE, col=sample)) + geom_line(stat="identity") + xlim(0,100)+ylim(0,30)+labs(title="Robustness - Secondary Extinction",x="Primary Extinction - (%) of species removed")
df3 %>% group_by(PrimExt, sample,nvert) %>% summarise(meanSE = mean(CumSecExt/nvert)) %>% ggplot(aes(x = 100*PrimExt/nvert, y = 100*meanSE, col=sample)) + geom_line(stat="identity") + xlim(0,100)+ylim(0,30)+labs(title="Robustness - Secondary Extinction",x="Primary Extinction - (%) of species removed",y="Cummulative Secondary Extinction")
df3 %>% group_by(PrimExt, sample,nvert) %>% summarise(meanSE = mean(CumSecExt/nvert)) %>% ggplot(aes(x = 100*PrimExt/nvert, y = 100*meanSE, col=sample)) + geom_line(stat="identity") + xlim(0,100)+ylim(0,30)+labs(title="Robustness - Secondary Extinction",x="Primary Extinction - (%) of species removed",y="Cummulative Secondary Extinction (%)")
p2 <- df3 %>% group_by(PrimExt, sample,nvert) %>% summarise(meanSE = mean(CumSecExt/nvert)) %>% ggplot(aes(x = 100*PrimExt/nvert, y = 100*meanSE, col=sample)) + geom_line(stat="identity") + xlim(0,100)+ylim(0,30)+labs(title="Robustness - Secondary Extinction",x="Primary Extinction - (%) of species removed",y="Cummulative Secondary Extinction (%)")
png("cummulativesecext.png",width=3200,height=1800,res=300)
print(p1)
print(p2)
dev.off()
savehistory("cummulativesecext.R")
