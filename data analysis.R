rm(list = ls())

library(lattice)
library(readr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)

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


mean(all$DSI[all$binary=="kin"]) #5.89
mean(all$DSI[all$binary=="non-kin"]) # 0.63

sd(all$DSI[all$binary=="kin"]) #16.34
sd(all$DSI[all$binary=="non-kin"]) #3.47

nonkintop1<-all %>%
  subset(binary == "non-kin" & order.of.partner == 1)

unique(nonkintop1$focal.id)

mean(all$focal.rank) #31.15
mean(nonkintop1$focal.rank) #29.62

mean(all$focal.age) #12.28
mean(nonkintop1$focal.age) #11.99

mean(all$focal.kin.available) #4.05
mean(nonkintop1$focal.kin.available) #3.12

mean(all$focal.connections) #10.07
mean(nonkintop1$focal.connections) #10.96


plot(as.numeric(all$per.nonkin.in.top3)~as.factor(all$focal.kin.available))
plot(as.numeric(all$per.nonkin.in.top3)~as.factor(all$focal.connections))


#kinship and DSI
top3<-all %>%
  subset(top3==T) %>%
  mutate(order.of.partner=as.factor(order.of.partner))

top10<-all %>%
  subset(order.of.partner <= 10 ) %>%
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

View(alllong[alllong$focal.id=="82A" & alllong$partner.id == "04N", c("groupyear","partner.id","DSI","delta.DSI", "order.of.partner","delta.order")])

flong <- alllong %>%
  subset ( group == "f" & dyad.total.years > 1 & meanDSI.over.years > 0 & top3==T)

sapply(flong, function(x) sum(is.na(x)))

mean(flong$meanDSI.over.years[flong$binary=="kin"]) #22.75
mean(flong$meanDSI.over.years[flong$binary=="non-kin"]) #4.91

mean(flong$sdDSI.over.years[flong$binary=="kin"]) #13.19
mean(flong$sdDSI.over.years[flong$binary=="non-kin"]) #5.67

mean(flong$delta.DSI[flong$binary=="kin" & !is.na(flong$delta.DSI)]) #3.53
mean(flong$delta.DSI[flong$binary=="non-kin"  & !is.na(flong$delta.DSI)]) #7.32

mean(abs(flong$delta.DSI[flong$binary=="kin" & !is.na(flong$delta.DSI)])) #15.04
mean(abs(flong$delta.DSI[flong$binary=="non-kin"  & !is.na(flong$delta.DSI)])) #9.49

mean(flong$delta.order[flong$binary=="kin" & !is.na(flong$delta.order)])
mean(flong$delta.order[flong$binary=="non-kin"  & !is.na(flong$delta.order)])

plot(data = flong, delta.DSI~as.factor(binary))
plot(data = flong, delta.order~as.factor(binary))

#death of mother
deadmother<-read.csv("~/Desktop/Non-kin cooperation data/pedigree.csv",header=T,na.strings = c("", "NA")) %>%
  select("ID","DOD","REMOVE.DATE") %>%
  subset(!is.na(DOD) | !is.na(REMOVE.DATE)) %>%
  mutate(DOD = str_replace(DOD,".*/.*/","")) %>%
  mutate(REMOVE.DATE = str_replace(REMOVE.DATE,".*/.*/","") ) %>%
  gather(dead.or.removed,year,DOD:REMOVE.DATE,na.rm=T) %>%
  mutate(dead.or.removed = ifelse(dead.or.removed == "DOD","dead","removed")) %>%
  setNames(c("focal.behavioral.mother","mother.dead.or.removed","year")) %>%
  mutate(year = as.numeric(year))

fdeadmother <- all %>%
  subset(group == "f")

fdeadmother<-left_join(fdeadmother,deadmother, by = c("focal.behavioral.mother","year"))

dummy<-fdeadmother %>%
  subset(!is.na(mother.dead.or.removed)) %>%
  select(focal.id,year) %>%
  setNames(c("focal.id","year.mother.dead.or.removed")) %>%
  unique()

fdeadmother <- left_join(fdeadmother,dummy,by="focal.id")

fdeadmother <- fdeadmother %>%
  mutate(mother.alive = ifelse(year.mother.dead.or.removed == year, "mother leaving", ifelse (year < year.mother.dead.or.removed, "mother present", ifelse (year > year.mother.dead.or.removed, "mother dead or removed", NA))))

View(fdeadmother[fdeadmother$focal.id=="14I",c(4,5,49:51)])

fdmanalysis<- fdeadmother %>%
  select (focal.id,year,per.nonkin.in.top3,mother.alive,focal.connections,focal.kin.available,focal.age,focal.ordinal.rank,focal.percofsex.dominanted,focal.rank) %>%
  unique() %>%
  subset(!is.na(mother.alive))

unique(fdmanalysis$focal.id)
