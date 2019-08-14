library(tidyverse)

args = commandArgs(trailingOnly=TRUE)
inputfile <- args[1]
inputfile<-"keystonepecies_specieswithmoreprey_secext_nodes_B1.dat"
dat <- read.csv(inputfile,sep="\t",header=FALSE)

network <- unlist(strsplit(unlist(strsplit(inputfile,"[.]"))[1],"_"))[5]
r<-unique(as.numeric(table(dat$V1)))

p1 <- dat %>% 
	group_by(V1) %>% 
	summarise(mean = mean(V2)) %>% 
	arrange(mean) %>%
        tail(40)%>%	
	ggplot(aes(x=reorder(V1,mean),y=mean))+
	geom_bar(stat="identity")+
	labs(x="Plant species",
	     y="Mean Number of Secondary Extinction Events",
	     title=paste0("Secondary Extinction ",network," network - ",r," realizations"))+
        coord_flip()+
	geom_text(aes(x=reorder(V1,mean),y=mean,label=signif(mean,3)),hjust=-0.1,size=3)

png(paste0("keystonespecies_secextinctiondistribution_",network,".png"),width=3377,height=1900,res=300)
print(p1)
dev.off()


dat2 <- dat %>% filter(V1 == "Briza_media")
plot(1:nrow(dat2),dat2$V2)


####

library(tidyverse)

B1networks <- system("ls Output*variable*B1.dat", intern = TRUE)
B8networks <- system("ls Output*variable*B8.dat", intern = TRUE)

dfB1 <- data.frame()
f <- read.csv2(B1networks[1])
namesB1 <- names(f)
for(f in B1networks){
  if(nrow(dfB1)==0){
    dfB1 <- read.csv2(f,skip = 1,header = FALSE)
  }else{
    dfB1 <- rbind(dfB1,
                  read.csv2(f,skip = 1,header = FALSE))
  }
}
names(dfB1) <- namesB1

dfB8 <- data.frame()
f <- read.csv2(B8networks[1])
namesB8 <- names(f)
for(f in B8networks){
  if(nrow(dfB8)==0){
    dfB8 <- read.csv2(f,skip = 1,header = FALSE)
  }else{
    dfB8 <- rbind(dfB8,
                  read.csv2(f,skip = 1,header = FALSE))
  }
}
names(dfB8) <- namesB8


p1<-dfB1 %>% 
  group_by(PrimExt) %>% 
  summarise(meanSecExt = mean(CumSecExt,na.rm=TRUE),
            sdSecExt = sd(CumSecExt,na.rm=TRUE)
  ) %>%
  ggplot(aes(x = PrimExt/66*100, y = meanSecExt/66*100)) +
  geom_line()



p8<-dfB8 %>% 
  group_by(PrimExt) %>% 
  summarise(meanSecExt = mean(CumSecExt,na.rm=TRUE),
            sdSecExt = sd(CumSecExt,na.rm=TRUE)
  ) %>%
  ggplot(aes(x = PrimExt/66*100, y = meanSecExt/66*100)) +
  geom_line()

aux_B1 <- dfB1 %>% 
  group_by(PrimExt) %>% 
  summarise(meanSecExt = mean(CumSecExt,na.rm=TRUE),
            sdSecExt = sd(CumSecExt,na.rm=TRUE)
  ) %>%
  mutate(network = "B1", elevation = 1)

aux_B8 <- dfB8 %>% 
  group_by(PrimExt) %>% 
  summarise(meanSecExt = mean(CumSecExt,na.rm=TRUE),
            sdSecExt = sd(CumSecExt,na.rm=TRUE)
  ) %>%
  mutate(network = "B8", elevation = 8)

aux <- rbind(aux_B1, aux_B8)

aux %>% ggplot(aes(x = PrimExt/66*100, y = meanSecExt/66*100, col = network)) +
  geom_line()
