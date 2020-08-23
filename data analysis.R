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

all <- do.call(rbind,lapply(filenames,function(x) {
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

sapply(all, function(x) sum(is.na(x)))


#compare means

plot(DSI ~ r, data=all)
plot(DSI ~ as.factor(binary), data=all)
plot(DSI ~ as.factor(relationship), data=all)

mean(all$DSI[all$binary=="kin"])
mean(all$DSI[all$binary=="non-kin"])
sd(all$DSI[all$binary=="kin"])
sd(all$DSI[all$binary=="non-kin"])

nonkintop1<-all %>%
  subset(binary == "non-kin" & order.of.partner == 1)

mean(all$focal.rank)
mean(nonkintop1$focal.rank)

mean(all$focal.age)
mean(nonkintop1$focal.age)

mean(all$focal.kin.available)
mean(nonkintop1$focal.kin.available)

mean(all$focal.connections)
mean(nonkintop1$focal.connections)

plot(as.numeric(per.nonkin.in.top3)~as.factor(focal.kin.available))
plot(as.numeric(per.nonkin.in.top3)~as.factor(focal.connections))

#no matter how many kin available (<=8), there were always non-kin top partner

#kinship and DSI
top3<-all %>%
  subset(top3==T) %>%
  mutate(order.of.partner=as.factor(order.of.partner))

top10<-all %>%
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


