rm(list=ls())
library(reshape)
library(igraph)
library(dplyr)

groupyears <-
  c( "f2010",
     "f2011",
     "f2012",
     "f2013",
     "f2014",
     "f2015",
     "f2016",
     "f2017",
     "hh2014",
     "hh2016",
     "kk2013",
     "kk2015",
     "kk2017",
     "r2015",
     "r2016",
     "s2011",
     "v2015",
     "v2016",
     "v2017"
  )

sna<-function (i) {
  dd<-read.csv(file=paste0("~/Desktop/Non-kin cooperation data/wrangled data/",i,"DSI.csv"),header=T) %>%
    mutate(DSI = ifelse(binary == "kin",DSI,-DSI)) %>%
    mutate(DSI = ifelse (binary == "kin" & DSI == 0,10000,DSI))%>%
    dplyr::select(c("focal.id","partner.id","DSI"))
  
  dd<-as.matrix(cast(dd, focal.id~partner.id,margins=c("partner.id","focal.id")))
  dd[is.na(dd)] <- 0
  
  G<-graph.adjacency(dd,mode="undirected",weighted=T) 
  
  E(G)$color <- ifelse (E(G)$weight ==10000,"orange",ifelse(E(G)$weight < 0,"blue","red"))
  E(G)$lty <- ifelse(E(G)$weight==10000,"dotdash","solid")
  E(G)$weight <- ifelse(E(G)$weight == 10000,1,abs(E(G)$weight))
  
  V(G)$label.cex <- 0.5
  
  
  pdf(paste0("~/Desktop/Non-kin cooperation data/graphs/social networks/",i,"sna.pdf"))
  
  plot.igraph(G,vertex.size=3,
              edge.width=E(G)$weight*0.1,
              vertex.frame.color="#ffffff")
  
  dev.off()
  
}

lapply(groupyears, sna)


sna.maternal<-function (i) {
  dd<-read.csv(file=paste0("~/Desktop/Non-kin cooperation data/wrangled data/",i,"DSI.csv"),header=T) %>%
    mutate(DSI = ifelse(r.mom>=0.125,DSI,-DSI)) %>%
    mutate(DSI = ifelse (r.mom>=0.125 & DSI == 0,10000,DSI))%>%
    dplyr::select(c("focal.id","partner.id","DSI"))
  
  dd<-as.matrix(cast(dd, focal.id~partner.id,margins=c("partner.id","focal.id")))
  dd[is.na(dd)] <- 0
  
  G<-graph.adjacency(dd,mode="undirected",weighted=T) 
  
  E(G)$color <- ifelse (E(G)$weight ==10000,"orange",ifelse(E(G)$weight < 0,"blue","red"))
  E(G)$lty <- ifelse(E(G)$weight==10000,"dotdash","solid")
  E(G)$weight <- ifelse(E(G)$weight == 10000,1,abs(E(G)$weight))
  
  V(G)$label.cex <- 0.5

  
  pdf(paste0("~/Desktop/Non-kin cooperation data/graphs/social networks/maternal",i,"sna.pdf"))
  
  plot.igraph(G,vertex.size=3,
              edge.width=E(G)$weight*0.1,
              vertex.frame.color="#ffffff")
  
  dev.off()
  
}

lapply(groupyears, sna.maternal)

