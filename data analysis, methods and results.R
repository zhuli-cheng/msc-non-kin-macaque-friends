rm(list = ls())

library(lattice)
library(readr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(lme4)
library(car)
library(sm)
library(blmeco)
library(lmtest)
library(cowplot)
library(glmmTMB)
library(DHARMa)
library(rmcorr)
library(mediation)
library(effsize)


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

migration <- all %>%
       dplyr::select (focal.id,group) %>%
       unique() %>%
       group_by(focal.id) %>%
       mutate(migration = n()) %>%
       subset(migration != 1)

all <- all %>%
  subset(!focal.id %in% migration$focal.id ) %>%
  subset(!partner.id %in% migration$focal.id ) %>%
  subset(!focal.behavioral.mother %in% migration$focal.id ) %>%
  subset(!partner.behavioral.mother%in% migration$focal.id ) 


all <- all %>%
  group_by(focal.id) %>%
  mutate(mean.DSI.gregariousness = mean(DSI)) %>%
  ungroup() %>%
  group_by(focal.id,year) %>%
  mutate(gregariousness.centerred = mean(DSI)) %>%
  ungroup()

all <- all %>% 
  mutate(dyad = paste(focal.id,partner.id,sep = "+")) %>%
  group_by(dyad) %>%
  mutate(dyad.total.years = n()) %>%
  ungroup()



#descriptives
all.desc<-all %>%
  dplyr::select(focal.id,group,year,focal.year.of.birth,focal.age,focal.kin.available,focal.maternalkin.available,focal.percofsex.dominanted,focal.hrs.focalfollowed,focal.total.scan,mean.DSI.gregariousness) %>%
  unique() %>%
  group_by(focal.id) %>% 
  mutate(focal.years = n()) 

nrow(all.desc) #monkey years
length(unique(all.desc$focal.id)) #monkeys
length(unique(all$groupyear)) #group years

range(all.desc$focal.age)
hist(all.desc$focal.age,breaks = 23)

hist(all.desc$focal.hrs.focalfollowed)

range(all.desc$focal.total.scan)
hist(all.desc$focal.total.scan,breaks=153)

hist(all.desc$mean.DSI.gregariousness)

ggplot(data = all, aes(x=log(r+0.001))) +
  geom_density() +
  facet_wrap(~group,scales="free_y")
  



#graphes 
p1<-ggplot(all, aes(x=DSI)) +
  geom_density()

p2<-ggplot(all, aes(x=DSI+0.001)) +
  geom_density()+
  scale_x_log10(labels = function(x) format(x, scientific = F))
#most pairs had no interactions 

p3<-ggplot(all.desc, aes(x=mean.DSI.gregariousness+0.001)) +
  geom_density()


p4<-ggplot(all, aes(x=DSI+0.001, color=binary)) +
  geom_density() +
  scale_x_log10(labels = function(x) format(x, scientific = F))
#kin dyads are more likely to have interactions and higher DSI, but non-kin dyad can also have very high DSI

plot_grid(p1,p2,p3,p4, labels = "AUTO")

p5<-ggplot(all, aes(x=grooming.rate.over.mean+0.001, color=binary)) +
  geom_density() +
  scale_x_log10(labels = function(x) format(x, scientific = F))

p6<-ggplot(all, aes(x=proximity.rate.over.mean+0.001, color=binary)) +
  geom_density()+
  scale_x_log10(labels = function(x) format(x, scientific = F))
#grooming is less likely to happen than proximity 

plot_grid(p4,p5,p6, labels = "AUTO")

ggplot(all, aes(x=DSI+0.001, color=relationship)) +
  geom_density() +
  scale_x_log10(labels = function(x) format(x, scientific = F))

#1.1 DSI ~ kinship
#1.1.1 DSI ~ r
ggplot(all,aes(x=r,y=DSI+0.001)) +
  geom_count(alpha=0.5) + 
  scale_y_log10(labels = function(x) format(x, scientific = F)) +
  facet_wrap(~group,scales="free_y")
  

xyplot(log(DSI+0.001)~r|focal.id,pch=16,cex=0.6, type=c("p","r"),data=all)
#the slopes are rather uniform

DSI.r.p <- glmer (DSI ~ r + (1|focal.id) + (1|partner.id), data=all, nAGQ=0,family=poisson)
dispersion_glmer(DSI.r.p) 
#overdispersion, try nb? 

DSI.r.nb <- glmer.nb (DSI ~ r + (1|focal.id) + (1|partner.id), data=all, nAGQ=0)
summary(DSI.r.nb)
drop1(DSI.r.nb, test = "Chisq")
# The exponentiated coefficients are also known as Incidence Rate Ratios (IRR)
fixed <- fixef(DSI.r.nb)
confintfixed <- confint(DSI.r.nb, parm = "beta_", method = "Wald") # Beware: The Wald method is less accurate but much, much faster.
IRR <- exp(cbind(fixed, confintfixed))
ratio<-IRR^(0.5-0.125)
ratio


#check assumptions: normality of intercepts and slopes; log of counts is linearly correlated to the predictor; dispersion
hist(intercep.nb<-coef(DSI.r.nb)$focal.id[,1],breaks=100)
dispersion_glmer(DSI.r.nb) #under dispersion 
qqPlot(residuals(DSI.r.nb))
plot(residuals(DSI.r.nb)~fitted(DSI.r.nb))
hist(coef(DSI.r.nb)$focal.id)
View(summary(DSI.r.nb))
summary(DSI.r.nb)[["coefficients"]]

#zero inflated 
100*sum(all$DSI == 0)/nrow(all) # more than 80% is zero! test Zero inflation
simulationOutput <- simulateResiduals(fittedModel = DSI.r.nb)
testZeroInflation(simulationOutput) #data is not really zero inflated when using negative binomial
plot(simulationOutput)


#test maternal/paternal kinship and age difference
DSI.r.dad <-glmer.nb (DSI ~ r.mom + r.dad + abs(age.diff) + (1|focal.id) + (1|partner.id), data=all, nAGQ=0)
dispersion_glmer(DSI.r.dad)
summary(DSI.r.dad)
drop1(DSI.r.dad,test = "Chisq")
hist(coef(DSI.r.dad)$focal.id)


#1.2 top3 ~ kinship

#1.2.1 top3 ~ binary
ggplot(all) + 
  geom_bar(aes(x = binary, fill = top3)) +
  facet_wrap(~group,scales="free_y")

Top3<-all %>%
  subset(top3==T) %>%
  mutate(order.of.partner=as.factor(order.of.partner))

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
}

ggplot(data = Top3, aes(x=order.of.partner, y=DSI+0.001) )  +
  geom_boxplot(aes(fill=binary)) +
  geom_point(aes(y=DSI+0.001, group=binary), position = position_dodge(width=0.75))+ 
  scale_y_log10(labels = function(x) format(x, scientific = F)) +
  facet_wrap( ~ order.of.partner, scales="free") + 
  xlab("Top Partners") + ylab("DSI") + ggtitle("All Group Years")+
  stat_summary(fun.data = give.n, geom = "text", fun = median, aes(group=binary),
                        position = position_dodge(width = 0.75),hjust = 1)+
  guides(fill=guide_legend(title="Dyad"))

top3.binary <- glmer(top3 ~ binary + (1|group) + (1|year), family = binomial, data=all)
summary(top3.binary)
drop1(top3.binary,test="Chisq")
#binary is significant predictor 

fixed <- fixef(top3.binary)
confintfixed <- confint(top3.binary, parm = "beta_", method = "Wald")
IRR <- exp(cbind(fixed, confintfixed))
IRR
#non-kin vs kin: odds of being top3 decreased to its 11% 

#check assumptions: no missing combinations;outliers; random intercepts 
chisq.test(all$top3,all$binary)$expected


#2 compare kin vs nonkin top3
ggplot(data = Top3, aes(x=DSI+0.001) )  +
  geom_density(aes(color=binary)) +
  facet_wrap( ~ order.of.partner, scales="free") +
  scale_x_log10(labels = function(x) format(x, scientific = F)) 


ggplot(data = Top3, aes(x=abs(age.diff), color = binary)) +
  geom_density() +
  facet_wrap( ~ order.of.partner, scales="free") 

ggplot(data = Top3[Top3$grooming.rate!=0,], aes(x=abs(groom.giving - groom.receiving)/(groom.giving + groom.receiving), color = binary)) +
  geom_density() 

#compare kin and non-kin top 1/2/3 partners
for (i in 3) {
  print(t.test(data = Top3[Top3$order.of.partner==i,], log(DSI) ~ binary))
  print(cohen.d(data = Top3[Top3$order.of.partner==i,], log(DSI) ~ binary))
  print(wilcox.test(data = Top3[Top3$order.of.partner==i,], abs(age.diff) ~ binary))
  print(wilcox.test(data = Top3[Top3$order.of.partner==i & Top3$grooming.rate!=0,], abs(groom.giving - groom.receiving)/(groom.giving + groom.receiving) ~ binary))
}


#2.2 stability 

f.top3.dyads <- unique(all$dyad[all$group == "f" & all$top3 == T] )

#graph
f.top3 <- all %>%
  subset (dyad %in% f.top3.dyads)

plist = lapply(split(f.top3, f.top3$focal.id), function(d) {
  ggplot(d, aes(year,DSI,color=binary)) + 
    geom_point() +
    facet_wrap(~ dyad) +
    theme_bw()
})
plist

#fluctuation
fluctuation <- f.top3 %>%
  subset(dyad.total.years > 1) %>%
  group_by(dyad) %>%
  mutate(mean.dyadic.DSI = mean(DSI)) %>%
  mutate(sd.dyadic.DSI = sd(DSI)) %>% 
  mutate(sd.DSI.over.mean.DSI = sd.dyadic.DSI/mean.dyadic.DSI) %>%
  ungroup() %>%
  dplyr::select(focal.id,partner.id,dyad.total.years,mean.dyadic.DSI,sd.dyadic.DSI,sd.DSI.over.mean.DSI,r,binary,top3) %>%
  unique()

sapply(fluctuation, function(x) sum(is.na(x)))

ggplot(data=fluctuation,aes(x=sd.DSI.over.mean.DSI,color=binary)) +
  geom_density()
#non-kin dyad fluctuates more, therefore are less stable... but do they go up or down? 


#years in top3
years.in.top3 <- f.top3 %>%
  group_by(dyad) %>%
  mutate(years.in.top3 = sum(top3)/n()) %>%
  ungroup() %>%
  dplyr::select(r,binary,relationship,focal.id,partner.id,dyad,years.in.top3) %>%
  unique()

ggplot(data=years.in.top3,aes(x=years.in.top3,color = binary)) +
  geom_density()

#disparity
disparity <- f.top3 %>%
  group_by(dyad) %>%
  mutate (groom.giving.total = sum(groom.giving)) %>%
  mutate (groom.receiving.total = sum(groom.receiving)) %>% 
  mutate(groom.total = groom.giving.total + groom.receiving.total) %>%
  subset(groom.total != 0) %>%
  mutate(disparity = abs(groom.giving.total - groom.receiving.total)/ (groom.giving.total + groom.receiving.total)) %>%
  dplyr::select(dyad,binary,groom.giving.total,groom.receiving.total,disparity) %>% 
  ungroup()%>%
  unique()

#post hoc: years in top3 and disparity
years.disparity <- full_join(years.in.top3,disparity)

ggplot(aes(y = years.in.top3, x = disparity, color = binary), data = years.disparity) +
  geom_count()


#3. predictors of non-kin partnership 
#add death of mother
deadmother<-
  read_tsv("~/Desktop/Non-kin cooperation data/PEDIGREE.txt") %>%
  setNames(c("ID","SEX","BIRTH","DOD","REMOVE.DATE","DAM","SIRE","BEHAVIORAL.MOM","CS.STATUS","GROUP.AT.BIRTH","CS.BIRTH.SEASON","Comments")) %>%
  dplyr::select("ID","DOD","REMOVE.DATE") %>%
  subset(!is.na(DOD) | !is.na(REMOVE.DATE))%>%
  mutate(DOD = str_replace(DOD,".*/.*/","")) %>%
  mutate(REMOVE.DATE = str_replace(REMOVE.DATE,".*/.*/","") ) %>%
  gather(dead.or.removed,year,DOD:REMOVE.DATE,na.rm=T) %>%
  mutate(dead.or.removed = ifelse(dead.or.removed == "DOD","dead","removed")) %>%
  setNames(c("focal.behavioral.mother","mother.dead.or.removed","year.mother.left")) %>%
  mutate(year.mother.left = as.numeric(year.mother.left))

predictors<- left_join(all,deadmother,by = c("focal.behavioral.mother")) %>%
  subset (group == "f") %>%
  mutate(mother.presence = ifelse(is.na(year.mother.left),"present",ifelse(year.mother.left == year, "leaving", ifelse (year < year.mother.left, "present", ifelse (year > year.mother.left, "left", NA)))))

mother.not.top1 <- predictors %>%
  subset(binary == "non-kin" & order.of.partner == 1 ) %>%
  subset(mother.presence == "present")

unique(mother.not.top1$focal.id)

predictors<- predictors %>%
  dplyr::select (focal.id,focal.behavioral.mother,group,year,per.kin.in.top3,top3.kin,per.nonkin.in.top3,top3.nonkin,mother.presence,mean.DSI.gregariousness,gregariousness.centerred,focal.connections,focal.kin.available,focal.maternalkin.available,focal.age,focal.ordinal.rank,focal.percofsex.dominanted,focal.rank) %>%
  unique() %>%
  subset(focal.kin.available>=3)

#visualisation: mother
predictors %>%
  ggplot(aes(year, per.nonkin.in.top3)) +
  geom_point(aes(color = mother.presence)) + 
  facet_wrap(~ focal.id) 

#modeling 
predictors <- predictors %>%
  mutate(top3.total = top3.kin + top3.nonkin) %>%
  subset(top3.total != 0)

predictors <- predictors %>%
  group_by (focal.id) %>%
  mutate(mean.age = mean(focal.age)) %>%
  mutate(mean.rank = mean(focal.percofsex.dominanted)) %>%
  mutate(mean.kina = mean(focal.kin.available)) %>%
  mutate(mean.connection = mean(focal.connections)) %>%
  ungroup() %>%
  mutate (age.centerred = focal.age - mean.age) %>%
  mutate (rank.centerred = focal.percofsex.dominanted - mean.rank) %>% 
  mutate (connections.centerred = focal.connections - mean.connection)  %>%
  mutate (kina.centerred = focal.kin.available - mean.kina)

#proportional binomial model

predictors.pb <- glmer(top3.nonkin/(top3.total)
                       ~ mother.presence 
                       + mean.age + age.centerred 
                       + mean.rank + rank.centerred 
                       + mean.kina + kina.centerred 
                       + mean.connection + connections.centerred 
                       + mean.DSI.gregariousness + gregariousness.centerred 
                       + (1|focal.id),
                       weights=top3.total,family=binomial,data=predictors)


vif(predictors.pb)

summary(predictors.pb)
drop1(predictors.pb,test="Chisq")

qqPlot(residuals(predictors.pb))
plot(residuals(predictors.pb)~fitted(predictors.pb))
hist(predictors.pb@u)

#correlation between age and kin available
age.kina <- predictors %>% 
  group_by(focal.id) %>%
  dplyr::select(focal.id,mean.age,mean.kina) %>%
  unique() %>%
  ungroup()

cor.test(age.kina$mean.age,age.kina$mean.kina)

ggplot(data= predictors, aes(x=focal.maternalkin.available,y=per.nonkin.in.top3)) +
  geom_count()

#the reason not doing all groups together is because of the collinearity of group and all other predictors. worth doing separate analysis for all groups? 
#4. others 


top10<-all %>%
  subset(order.of.partner <= 10 ) %>%
  mutate(order.of.partner=as.factor(order.of.partner))
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

#non-kin dyads: characteristics, i.e. similar age or rank?
#grooming equality

