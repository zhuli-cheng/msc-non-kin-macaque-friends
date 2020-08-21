library(reshape)
library(igraph)
library(dplyr)

sna<-
rm(list=ls())
F2010sna<-read.csv("~/Desktop/Non-kin cooperation data/wrangled data/f2010DSI.csv",header=T)%>%
  select(c("focal.id","partner.id","DSI"))


F2010sna$groom.giving<-as.numeric(F2010sna$DSI)

F2010sna<-as.matrix(cast(F2010sna, focal.id~partner.id,margins=c("partner.id","focal.id")))

F2010sna[is.na(F2010sna)] <- 0

monkey.matrix=graph.adjacency(F2010sna,mode="undirected",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)

#changes size of labels of vertex (nodes)
V(monkey.matrix)$label.cex <- 1

#vary edge width by strength of association (weighted)
sociogram3 <- plot.igraph(monkey.matrix, vertex.size=3,
                          edge.width=E(monkey.matrix)$weight*0.1,
                          edge.color="black", edge.arrow.size = 0.5)


