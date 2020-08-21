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
  gysna<-read.csv(file=paste0("~/Desktop/Non-kin cooperation data/wrangled data/",i,"DSI.csv"),header=T) %>%
                    select(c("focal.id","partner.id","DSI")) %>%
    mutate(DSI=as.numeric(DSI))
  
  gysna<-as.matrix(cast(gysna, focal.id~partner.id,margins=c("partner.id","focal.id")))
  
  gysna[is.na(gysna)] <- 0
  
  monkey.matrix=graph.adjacency(gysna,mode="undirected",weighted=T)
  
  V(monkey.matrix)$label.cex <- 0.5
  
  pdf(paste0("~/Desktop/Non-kin cooperation data/graphs/",i,"sna.pdf"))
  sociogram3 <- plot.igraph(monkey.matrix, vertex.size=3,
                            edge.width=E(monkey.matrix)$weight*0.1,
                            edge.color="black", edge.arrow.size = 0.5)
  dev.off()
  
}



lapply(groupyears, sna)


