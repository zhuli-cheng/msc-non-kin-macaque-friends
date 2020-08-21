rm(list = ls())

library(lattice)
library(readr)
library(reshape2)
library(ggplot2)

#loading data
filenames <-
  list.files(
    "~/Desktop/Non-kin cooperation data/wrangled data",
    pattern = "*.csv",
    full.names = TRUE
  )

allDSI <- do.call(rbind,lapply(filenames,function(x) {
  read_csv(
    x,
    col_types = cols(
      group = "f",
      focal.sex = "f",
      partner.sex = "f",
      focal.group.at.birth = "f",
      partner.group.at.birth = "f"
    )
  )
}))

sapply(allDSI, function(x) sum(is.na(x)))

#number of kin available
#number of non-kin top3 partners
#what are the chance for kin to be top3 and non-kin top3, on the level of how many kin available 

#percentage of non-kin top partner and number of kin and connections available
attach(allDSI)
boxplot(as.numeric(per.nonkin.in.top3)~as.numeric(focal.kin.available))
boxplot(as.numeric(per.nonkin.in.top3)~as.numeric(focal.connections))

#no matter how many kin available (<=8), there were always non-kin top partner

#kinship and DSI
top3<-allDSI %>%
  subset(top3==T) %>%
  mutate(order.of.partner=as.factor(order.of.partner))

top10<-allDSI %>%
  subset(order.of.partner <= 10 & order.of.partner > 0 ) %>%
  mutate(order.of.partner=as.factor(order.of.partner))

p <- ggplot(data = top3, aes(x=order.of.partner, y=DSI)) 
p <- p + geom_boxplot(aes(fill=binary))
#p <- p + geom_jitter()
p <- p + geom_point(aes(y=DSI, group=binary), position = position_dodge(width=0.75))
p <- p + facet_wrap( ~ order.of.partner, scales="free")
p <- p + xlab("Top Partners") + ylab("DSI") + ggtitle("All Group Years")
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
}
p <- p + stat_summary(fun.data = give.n, geom = "text", fun.y = median, aes(group=binary),
                      position = position_dodge(width = 0.75),hjust = 1)
p <- p + guides(fill=guide_legend(title="Dyad"))
p 

q <- ggplot(data = top10, aes(x=order.of.partner, y=DSI)) 
q <- q + geom_boxplot(aes(fill=binary))
#q <- q + geom_jitter()
q <- q + geom_point(aes(y=DSI, group=binary), position = position_dodge(width=0.75))
q <- q + facet_wrap( ~ order.of.partner, scales="free")
q <- q + xlab("Top Partners") + ylab("DSI") + ggtitle("All Group Years")
q <- q + guides(fill=guide_legend(title="Dyad"))
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
}
q <- q + stat_summary(fun.data = give.n, geom = "text", fun.y = median, aes(group=binary),
             position = position_dodge(width = 0.75),hjust = 1)
q 

mean(allDSI$DSI[allDSI$binary=="kin"])
mean(allDSI$DSI[allDSI$binary=="non-kin"])

nonkintop1<-allDSI %>%
  subset(binary == "non-kin" & order.of.partner == 1)

mean(nonkintop1$focal.kin.available)
mean(allDSI$focal.kin.available)

mean(nonkintop1$focal.connections)
mean(allDSI$focal.connections)
