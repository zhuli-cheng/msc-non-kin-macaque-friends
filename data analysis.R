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

p3 <- ggplot(data = top3, aes(x=order.of.partner, y=DSI)) 
p3 <- p3 + geom_boxplot(aes(fill=binary))
#p3 <- p3 + geom_jitter()
p3 <- p3 + geom_point(aes(y=DSI, group=binary), position = position_dodge(width=0.75))
p3 <- p3 + facet_wrap( ~ order.of.partner, scales="free")
p3 <- p3 + xlab("Top Partners") + ylab("DSI") + ggtitle("All Group Years")
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
}
p3 <- p3 + stat_summary(fun.data = give.n, geom = "text", fun.y = median, aes(group=binary),
                      position = position_dodge(width = 0.75),hjust = 1)
p3 <- p3 + guides(fill=guide_legend(title="Dyad"))
p3

p10 <- ggplot(data = top10, aes(x=order.of.partner, y=DSI)) 
p10 <- p10 + geom_boxplot(aes(fill=binary))
#p10 <- p10 + geom_jitter()
p10 <- p10 + geom_point(aes(y=DSI, group=binary), position = position_dodge(width=0.75))
p10 <- p10 + facet_wrap( ~ order.of.partner, scales="free")
p10 <- p10 + xlab("Top Partners") + ylab("DSI") + ggtitle("All Group Years")
p10 <- p10 + guides(fill=guide_legend(title="Dyad"))
give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
}
p10 <- p10 + stat_summary(fun.data = give.n, geom = "text", fun.y = median, aes(group=binary),
             position = position_dodge(width = 0.75),hjust = 1)
p10 

#longitudinal analysis 
alllong <- all %>%
  group_by(focal.id,partner.id) %>%
  mutate(dyad.total.years = n()) %>%
  mutate(meanDSI.over.years = mean(DSI)) %>%
  mutate(sdDSI.over.years = sd(DSI)) %>% 
  mutate(delta.DSI = c(NA, diff(DSI))) %>%
  mutate(delta.order = c(NA,diff(order.of.partner))) %>%
  ungroup() %>%
  select (groupyear:DSI,dyad.total.years:delta.DSI,focal.connections:order.of.partner,delta.order,top3:partner.percofsex.dominanted)

View(alllong[focal.id=="82A" & partner.id == "04N", c("groupyear","partner.id","DSI","delta.DSI", "order.of.partner","delta.order")])

flong <- alllong %>%
  subset ( group == "f" & dyad.total.years > 1 & meanDSI.over.years > 0 & top3==T)

sapply(flong, function(x) sum(is.na(x)))

mean(flong$meanDSI.over.years[flong$binary=="kin"])
mean(flong$meanDSI.over.years[flong$binary=="non-kin"])

mean(flong$sdDSI.over.years[flong$binary=="kin"])
mean(flong$sdDSI.over.years[flong$binary=="non-kin"])

mean(flong$delta.DSI[flong$binary=="kin" & !is.na(flong$delta.DSI)])
mean(flong$delta.DSI[flong$binary=="non-kin"  & !is.na(flong$delta.DSI)])

mean(abs(flong$delta.DSI[flong$binary=="kin" & !is.na(flong$delta.DSI)]))
mean(abs(flong$delta.DSI[flong$binary=="non-kin"  & !is.na(flong$delta.DSI)]))

mean(flong$delta.order[flong$binary=="kin" & !is.na(flong$delta.order)])
mean(flong$delta.order[flong$binary=="non-kin"  & !is.na(flong$delta.order)])

plot(data = flong, meanDSI.over.years~as.factor(binary))
plot(data = flong, sdDSI.over.years~as.factor(binary))



