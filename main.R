library(igraph)
library(tidyverse)

source("secext.R");
setwd("/home/charles/ETH/camille/secondaryextinction");
load("igraph_ls.Rdata")

nets <- names(igraph_ls)
nnets <- length(nets)

for(n in 6:nnets){
  originalnet<-igraph_ls[[n]]
  nplants <- sum(degree(originalnet,mode="in")==0)
  realizations<-nplants*(nplants-1)
  for(r in 1:realizations){
   	nrealizations<-realizations;
   	cat(paste(n,r,sep=" "),sep="\n");
   	set.seed(r);
   	secondaryextinction(originalnet,1,r,n);#preyfirst
#   	secondaryextinction(originalnet,2,r,n);#predatorfirst
  }
}
