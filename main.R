library(igraph)

source("secondaryextinction/secext.R");
setwd(".");
load("igraph_ls.Rdata")

nets <- names(igraph_ls)
nnets <- length(nets)
realizations<-100
for(n in 1:nnets){
   for(r in 1:realizations){
   	originalnet<-igraph_ls[[n]]
   	nrealizations<-realizations;
   	cat(paste(n,r,sep=" "),sep="\n");
   	set.seed(r);
   	secondaryextinction(originalnet,1,r,n);#preyfirst
#   	secondaryextinction(originalnet,2,r,n);#predatorfirst
   }
}
