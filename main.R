library(igraph)

source("./secext.R");
setwd(".");
nets<-c("Arctic","Antarctic");
realizations<-c(19600,343396);
realizations<-c(10,10)
for(n in 1:length(nets)){
	for(r in 1:realizations[n]){
		web<-nets[n];
		netfilename<-paste("./",nets[n],".net",sep="");
		originalnet<-read.graph(netfilename,format="pajek");
		nrealizations<-realizations[n];
		cat(paste(n,r,sep=" "),sep="\n");
		set.seed(r);
		secondaryextinction(originalnet,1,r,n);#preyfirst
		secondaryextinction(originalnet,2,r,n);#predatorfirst
	}
}
