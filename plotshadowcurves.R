#To install any of these packages just run: install.packages("name-of-package)
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
  

#how to run

#First read your data into a matrix format
#Each line represents an experiment
#Each column represents a time step
data1<-runif(20)*2;
sd1<-runif(20)*0.1;
data2<-runif(20)*3;
sd2<-runif(20)*0.15;
data3<-runif(20)*1;
sd3<-runif(20)*0.25;
#In this example we have mean and standard deviation values for 3 experiments with 20 time steps each one
mymean<-as.matrix(rbind(data1,data2,data3))
mysd<-as.matrix(rbind(sd1,sd2,sd3))

#Then call the function plotShadow. The only mandatory parameters are the matrix of mean values and the matrix os std. deviation values
plotShadow(mymean,mysd)
#But you can also customize the level of transparency (alpha), the x and y labels (myxlab, myylab), the title of the plot (mymain), and the name of the output file (filename) 
plotShadow(mymean,mysd,myalpha=0.3,mylwd=2,myxlab="Time",myylab="Fluorecence",mymain="Comparison",filename="shadowcurve.png")
  
  
