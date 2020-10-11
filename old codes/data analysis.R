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


#kk2013 groomings are NA
#r2016 grooming a lot of NA and 0
#v2017 proximity scan number?
#r2016 proximity scan numbers very low (0-4)

#kk2015 5H4 self grooming
#kk2017DSI order not correct
#kk2017 and v2017 without idcode column

#descriptives
all.desc<-all %>%
  dplyr::select(focal.id,group,year,focal.year.of.birth,focal.age,focal.kin.available,focal.percofsex.dominanted,focal.hrs.focalfollowed,focal.total.scan,mean.DSI.gregariousness) %>%
  unique() %>%
  group_by(focal.id) %>% 
  mutate(focal.years = n()) 

nrow(all.desc) #monkey years
length(unique(all.desc$focal.id)) #monkeys
length(unique(all$groupyear)) #group years

range(all.desc$focal.age)
hist(all.desc$focal.age,breaks = 23)

range(all.desc$focal.years)
hist(all.desc$focal.years,breaks=8)

hist(all.desc$focal.percofsex.dominanted,breaks = 101)

hist(all.desc$focal.hrs.focalfollowed)

range(all.desc$focal.total.scan)
hist(all.desc$focal.total.scan,breaks=153)

hist(all.desc$mean.DSI.gregariousness)


#graphes 

p1<-ggplot(all, aes(x=DSI)) +
  geom_density()

p2<-ggplot(all, aes(x=DSI+0.001)) +
  geom_density()+
  scale_x_log10(labels = function(x) format(x, scientific = F))
#most pairs had no interactions 

p3<-ggplot(all.desc, aes(x=mean.DSI.gregariousness+0.001)) +
  geom_density()

#females differ in gregariousness, some had no interaction at all

p4<-ggplot(all, aes(x=DSI+0.001, color=binary)) +
  geom_density() +
  scale_x_log10(labels = function(x) format(x, scientific = F))
#kin dyads are more likely to have interactions and higher DSI, but non-kin dyad can also have very high DSI

plot_grid(p1,p2,p4,p3, labels = "AUTO")

#1.1 DSI ~ kinship
#1.1.1 DSI ~ r
ggplot(all,aes(x=r,y=DSI+0.001)) +
  geom_count(alpha=0.5) + 
  scale_y_log10(labels = function(x) format(x, scientific = F)) +
  facet_wrap(~group,scales="free_y")
  

xyplot(log(DSI+0.001)~r|focal.id,pch=16,cex=0.6, type=c("p","r"),data=all)
#the slopes are rather uniform

DSI.r.p1 <- glmer (DSI ~ r + (r|focal.id) + (r|partner.id), data=all, nAGQ=0,family=poisson)
summary(DSI.r.p1)
qqPlot(residuals(DSI.r.p1))
plot(residuals(DSI.r.p1)~fitted(DSI.r.p1))
a<-coef(DSI.r.p1)$focal.id
hist(a)
View(DSI.r.p1)

#DSI is positively correlated (and linearly when logged) with r

DSI.r.p <- glmer (DSI ~ r + (1|focal.id) + (1|partner.id), data=all, nAGQ=0,family=poisson)
anova(DSI.r.p,DSI.r.p1)

dispersion_glmer(DSI.r.p) #overdispersion, try nb? 

DSI.r.nb <- glmer.nb (DSI ~ r + (1|focal.id) + (1|partner.id), data=all, nAGQ=0)
dispersion_glmer(DSI.r.nb) #under dispersion 
summary(DSI.r.nb)
qqPlot(residuals(DSI.r.nb))
plot(residuals(DSI.r.nb)~fitted(DSI.r.nb))
hist(coef(DSI.r.nb)$focal.id)


#try OLRE
DSI.r.OLRE <-glmer (DSI ~ r + (1|focal.id) + (1|partner.id) + (1|X1), data=all, nAGQ=0,family=poisson)
dispersion_glmer(DSI.r.OLRE)
View(summary(DSI.r.OLRE))
drop1(DSI.r.OLRE,test = "Chisq")

lrtest(DSI.r.OLRE,DSI.r.nb)
#DSI.r.nb is better

#test association and age difference
DSI.r.dad <-glmer.nb (DSI ~ r.assoc +  abs(age.diff) + r.minus.assoc + (1|focal.id) + (1|partner.id), data=all, nAGQ=0)
dispersion_glmer(DSI.r.dad)
View(summary(DSI.r.dad))
drop1(DSI.r.dad,test = "Chisq")

DSI.r.dad1 <-glmer.nb (DSI ~ r.mom + r.dad + abs(age.diff) + (1|focal.id) + (1|partner.id), data=all, nAGQ=0)
dispersion_glmer(DSI.r.dad3)
summary(DSI.r.dad1)
drop1(DSI.r.dad1,test = "Chisq")

lrtest(DSI.r.OLRE,DSI.r.nb)

100*sum(all$DSI == 0)/nrow(all) # more than 80% is zero! test Zero inflation
simulationOutput <- simulateResiduals(fittedModel = DSI.r.nb)
testZeroInflation(simulationOutput) #data is not really zero inflated when using negative binomial
plot(simulationOutput)


#because of over dispersion, perhaps use negative binomial distribution? (quasi poisson cannot be used on mixed models)
#random slope? DSI.r.nb.m1 <- glmer.nb (DSI ~ r + (r|focal.id), data=all, nAGQ=0)

View(summary(DSI.r.nb))
 summary(DSI.r.nb)[["coefficients"]]

fixed <- fixef(DSI.r.nb)
confintfixed <- confint(DSI.r.nb, parm = "beta_", method = "Wald") # Beware: The Wald method is less accurate but much, much faster.

# The exponentiated coefficients are also known as Incidence Rate Ratios (IRR)
IRR <- exp(cbind(fixed, confintfixed))
IRR
exp(8.54/8)

#check assumptions: normality of intercepts and slopes; log of counts is linearly correlated to the predictor; dispersion

hist(intercep.nb<-coef(DSI.r.nb)$focal.id[,1],breaks=100)



#1.1.2 DSI ~ binary

p5<-ggplot(all, aes(x=grooming.rate.over.mean+0.001, color=binary)) +
  geom_density() +
  scale_x_log10(labels = function(x) format(x, scientific = F))

p6<-ggplot(all, aes(x=proximity.rate.over.mean+0.001, color=binary)) +
  geom_density()+
  scale_x_log10(labels = function(x) format(x, scientific = F))
#grooming is less likely to happen than proximity 

plot_grid(p4,p5,p6, labels = "AUTO")






DSI.binary <- glmer (DSI ~ binary + (1|focal.id) + (1|partner.id), data=all, nAGQ=0,family=poisson)
dispersion_glmer(DSI.binary)

DSI.binary.nb <- glmer.nb (DSI ~ binary + (1|focal.id) + (1|partner.id), data=all, nAGQ=0,family=poisson)
dispersion_glmer(DSI.binary.nb)
hist(coef(DSI.binary.nb)$focal.id[,1])

lrtest(DSI.binary,DSI.binary.nb)

summary(DSI.binary)[["coefficients"]]
summary(DSI.binary.nb)


#1.1.3 DSI ~ relationship
ggplot(all, aes(x=DSI+0.001, color=relationship)) +
  geom_density() +
  scale_x_log10(labels = function(x) format(x, scientific = F))

#1.2 top3 ~ kinship

# 1.2.1 top3 ~ r

ggplot(all) + 
  geom_count(aes(x = r, y = top3)) +
  facet_wrap(~group,scales="free_y")

top3.r <- glmer(top3 ~ r + (r|group) + (r|year), family = binomial, data=all)

coef(top3.r)
summary(top3.r)
drop1(top3.r,test="Chisq")
#r is significant predictor 
exp(8.296/8)
exp(8.296/4)
exp(8.296/2)

fixed <- fixef(top3.r)
confintfixed <- confint(top3.r, parm = "beta_", method = "Wald")
IRR <- exp(cbind(fixed, confintfixed))
IRR


#check assumptions: linearity in the logit;outliers; random intercepts 

top3.r.lin<-glmer(top3 ~ r + r*log(r+0.001)+(r|group) + (r|year), family = binomial, data=all)
summary(top3.r.lin)
#linearity in the logit is violated!

#1.2.2 top3 ~ binary
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
  geom_count(aes(y=DSI+0.001, group=binary), position = position_dodge(width=0.75))+ 
  scale_y_log10(labels = function(x) format(x, scientific = F)) +
  facet_wrap( ~ order.of.partner, scales="free") + 
  xlab("Top Partners") + ylab("DSI") + ggtitle("All Group Years")+
  stat_summary(fun.data = give.n, geom = "text", fun = median, aes(group=binary),
                        position = position_dodge(width = 0.75),hjust = 1)+
  guides(fill=guide_legend(title="Dyad"))

ggplot(data = Top3, aes(x=DSI+0.001) )  +
  geom_density(aes(color=binary)) +
  facet_wrap( ~ order.of.partner, scales="free") +
  scale_x_log10(labels = function(x) format(x, scientific = F)) 


ggplot(data = Top3, aes(x=abs(age.diff), color = binary)) +
  geom_density() +
  facet_wrap( ~ order.of.partner, scales="free") 

ggplot(data = Top3[Top3$grooming.rate!=0,], aes(x=abs(groom.giving - groom.receiving)/(groom.giving + groom.receiving + 0.001), color = binary)) +
  geom_density() 

top3.binary <- glmer(top3 ~ binary + (binary|group) + (binary|year), family = binomial, data=all)

coef(top3.binary)
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

#compare kin and non-kin top 1/2/3 partners
for (i in 3) {
  print(t.test(data = Top3[Top3$order.of.partner==i,], log(DSI) ~ binary))
  print(cohen.d(data = Top3[Top3$order.of.partner==i,], log(DSI) ~ binary))
  print(t.test(data = Top3[Top3$order.of.partner==i,], abs(age.diff) ~ binary))
  print(cohen.d(data = Top3[Top3$order.of.partner==i,], abs(age.diff) ~ binary))
}


#1.2.3 top3 ~ relationships
ggplot(all) + 
  geom_bar(aes(x = relationship, fill = top3)) +
  facet_wrap(~group,scales="free_y")

#2. longitudinal - F group

all.long <- all %>%
  group_by(focal.id,partner.id) %>%
  mutate(dyad.total.years = n()) %>%
  mutate(mean.dyadic.DSI = mean(DSI)) %>%
  mutate(sd.dyadic.DSI = sd(DSI)) %>% 
  mutate(sd.DSI.over.mean.DSI = sd.dyadic.DSI/mean.dyadic.DSI) %>%
  mutate(delta.DSI = c(NA, diff(DSI))) %>%
  mutate(delta.order = c(NA,diff(order.of.partner))) %>%
  ungroup() %>%
  dplyr::select(groupyear:DSI,mean.DSI.gregariousness:delta.DSI,focal.connections:order.of.partner,delta.order,top3:partner.percofsex.dominanted)

flong.top3 <- all.long %>%
  subset ( group == "f" & top3 == T & dyad.total.years > 1 ) %>%
  dplyr::select(focal.id,partner.id,dyad.total.years,mean.dyadic.DSI,sd.dyadic.DSI,sd.DSI.over.mean.DSI,r,binary,top3) %>%
  unique()


sapply(flong.top3, function(x) sum(is.na(x)))

ggplot(data=flong.top3,aes(x=r,y=mean.dyadic.DSI)) +
  geom_count()
ggplot(data=flong.top3,aes(x=r,y=sd.dyadic.DSI)) +
  geom_count()
ggplot(data=flong.top3,aes(x=r,y=sd.DSI.over.mean.DSI)) +
  geom_count()

p7<-ggplot(data=flong.top3,aes(x=mean.dyadic.DSI,color=binary)) +
  geom_density()
p8<-ggplot(data=flong.top3,aes(x=sd.dyadic.DSI,color=binary)) +
  geom_density()
p9<-ggplot(data=flong.top3,aes(x=sd.DSI.over.mean.DSI,color=binary)) +
  geom_density()
#non-kin dyad fluctuates more, therefore are less stable... but do they go up or down? 
plot_grid(p7,p8,p9, labels = "AUTO")

#trend
flong.top3.dummy<-flong.top3 %>%
  mutate(dyad = paste(focal.id,partner.id,sep = "+"))

flong.yearly<-all.long %>%
  mutate(dyad = paste(focal.id,partner.id,sep = "+")) %>%
  subset(dyad %in% flong.top3.dummy$dyad)


plist = lapply(split(flong.yearly, flong.yearly$focal.id), function(d) {
  ggplot(d, aes(year,DSI,color=binary)) + 
    geom_point() +
    facet_wrap(~ dyad) +
    theme_bw()
})
plist
#years in top3
flong.yearly <- flong.yearly %>%
  group_by(dyad) %>%
  mutate(years.in.top3 = sum(top3)/n()) %>%
  ungroup()

Top3.yearly <- flong.yearly %>%
  dplyr::select(r,binary,relationship,focal.id,partner.id,dyad,years.in.top3) %>%
  unique()

ggplot(data=Top3.yearly,aes(x=years.in.top3,color = binary)) +
  geom_density()

p10<-ggplot(data=flong.yearly,aes(x=delta.DSI/mean.dyadic.DSI,color=binary,na.rm = TRUE)) +
  geom_density()
p11<-ggplot(data=flong.yearly,aes(y=delta.DSI/mean.dyadic.DSI,x=r,na.rm = TRUE,color=binary)) +
  geom_count()

p12<-ggplot(data=flong.yearly,aes(x=delta.order,color=binary,na.rm = TRUE)) +
  geom_density()
p13<-ggplot(data=flong.yearly,aes(y=delta.order,x=r,na.rm = TRUE,color=binary)) +
  geom_count()
#both kin and non-kin dyads have no tendency of having higher or lower DSI over the years.
#also in partner order, they fluctuates rather than going up or down
plot_grid(p10,p11,p12,p13, labels = "AUTO")

#years ~ reciprocity

reciprocity <- flong.yearly %>%
  group_by(dyad) %>%
  mutate (groom.giving.total = sum(groom.giving)) %>%
  mutate (groom.receiving.total = sum(groom.receiving)) %>% 
  mutate(groom.total = groom.giving.total + groom.receiving.total) %>%
  subset(groom.total != 0) %>%
  mutate(disparity = abs(groom.giving.total - groom.receiving.total)/ (groom.giving.total + groom.receiving.total)) %>%
  dplyr::select(dyad,binary,groom.giving.total,groom.receiving.total,disparity,years.in.top3) %>% 
  ungroup()%>%
  unique()

ggplot(aes(y = years.in.top3, x = disparity, color = binary), data = reciprocity) +
  geom_count()

ggplot(aes(x = years.in.top3, color = binary), data = reciprocity) +
  geom_density()

ggplot(aes(x = disparity, color = binary), data = reciprocity) +
  geom_density()

model <- lm(years.in.top3~disparity, data = reciprocity)

summary(model)

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

all.predictors<- left_join(all,deadmother,by = c("focal.behavioral.mother"))

all.predictors <- all.predictors %>%
  mutate(mother.alive = ifelse(is.na(year.mother.left),"mother alive",ifelse(year.mother.left == year, "mother leaving", ifelse (year < year.mother.left, "before mother left", ifelse (year > year.mother.left, "mother left", NA)))))
#assuming that all the females without death or removal record are still alive AND with their daughters in the same social group! 

all.predictors.analysis <- all.predictors %>%
  dplyr::select (focal.id,focal.behavioral.mother,group,year,per.kin.in.top3,top3.kin,per.nonkin.in.top3,top3.nonkin,mother.alive,mean.DSI.gregariousness,gregariousness.centerred,focal.connections,focal.kin.available,focal.age,focal.ordinal.rank,focal.percofsex.dominanted,focal.rank) %>%
  unique()

all.predictors.analysis <- all.predictors.analysis %>%
  subset(focal.kin.available>=3)

#visualisations

all.predictors.analysis %>%
  subset(group == "f") %>%
  ggplot(aes(year, per.nonkin.in.top3)) +
  geom_point(aes(color = mother.alive)) + 
  facet_wrap(~ focal.id) 

ggplot(all.predictors.analysis,aes(x=mother.alive,y=per.nonkin.in.top3)) +
  geom_count() 

ggplot(all.predictors.analysis,aes(x=mean.DSI.gregariousness,y=per.nonkin.in.top3)) +
  geom_count() 

ggplot(all.predictors.analysis,aes(x=focal.connections,y=per.nonkin.in.top3)) +
  geom_count() 

ggplot(all.predictors.analysis,aes(x=focal.kin.available,y=per.nonkin.in.top3)) +
  geom_count() 

ggplot(all.predictors.analysis,aes(x=focal.age,y=per.nonkin.in.top3)) +
  geom_count() 

ggplot(all.predictors.analysis,aes(x=focal.percofsex.dominanted,y=per.nonkin.in.top3)) +
  geom_count() 

ggplot(all.predictors.analysis,aes(x=factor(focal.ordinal.rank,level = c("0","L","M","H")),y=per.nonkin.in.top3)) +
  geom_count() 
  
b <- all.predictors %>%
  subset(binary == "non-kin" & order.of.partner == 1 ) %>%
  subset(mother.alive == "mother alive"| mother.alive == "before mother left")


unique(b$focal.id)

#modeling 


all.predictors.analysis <- all.predictors.analysis %>%
  mutate(top3.total = top3.kin + top3.nonkin) %>%
  subset(top3.total != 0)

all.predictors.analysis <- all.predictors.analysis %>%
  mutate (logit = log((per.nonkin.in.top3+0.001)/(per.kin.in.top3 + 0.001)))

all.predictors.analysis <- all.predictors.analysis %>%
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

f.predictors.analysis <- all.predictors.analysis %>%
  subset(group == "f")

#pairwise correlations between: age, rank, kin available and connections 
age.rank<-rmcorr(participant=as.factor(focal.id),measure1=focal.age,measure2=focal.rank,dataset=f.predictors.analysis,CIs="bootstrap")
plot(age.rank,overall=T)
age.rank
#negatively correlated 

age.kin<-rmcorr(participant=as.factor(focal.id),measure1=focal.age,measure2=focal.kin.available,dataset=f.predictors.analysis,CIs="bootstrap")
plot(age.kin,overall=T)
age.kin
#not correlated 

age.connection<-rmcorr(participant=as.factor(focal.id),measure1=focal.age,measure2=focal.connections,dataset=f.predictors.analysis,CIs="bootstrap")
plot(age.connection,overall=T)
age.connection
#negatively correlated 

rank.kin<-rmcorr(participant=as.factor(focal.id),measure1=focal.rank,measure2=focal.kin.available,dataset=f.predictors.analysis,CIs="bootstrap")
plot(rank.kin,overall=T)
rank.kin
#positively correlated 

rank.connection<-rmcorr(participant=as.factor(focal.id),measure1=focal.rank,measure2=focal.connections,dataset=f.predictors.analysis,CIs="bootstrap")
plot(rank.connection,overall=T)
rank.connection
#negatively correlated 

kin.connection<-rmcorr(participant=as.factor(focal.id),measure1=focal.kin.available,measure2=focal.connections,dataset=f.predictors.analysis,CIs="bootstrap")
plot(kin.connection,overall=T)
kin.connection
#negatively correlated 

top3.age<-rmcorr(participant=as.factor(focal.id),measure1=per.nonkin.in.top3,measure2=focal.age,dataset=f.predictors.analysis,CIs="bootstrap")
plot(top3.age,overall=T)
top3.age
#negatively correlated 

top3.rank<-rmcorr(participant=as.factor(focal.id),measure1=per.nonkin.in.top3,measure2=focal.percofsex.dominanted,dataset=f.predictors.analysis,CIs="bootstrap")
plot(top3.rank,overall=T)
top3.rank
#not correlated 

top3.kin<-rmcorr(participant=as.factor(focal.id),measure1=per.nonkin.in.top3,measure2=focal.kin.available,dataset=f.predictors.analysis,CIs="bootstrap")
plot(top3.kin,overall=T)
top3.kin
#negatively correlated 

top3.connection<-rmcorr(participant=as.factor(focal.id),measure1=per.nonkin.in.top3,measure2=focal.connections,dataset=f.predictors.analysis,CIs="bootstrap")
plot(top3.connection,overall=T)
top3.connection
#not correlated 

#linear model
predictors <- lmer(per.nonkin.in.top3 ~ mother.alive + mean.age + age.centerred + mean.rank + rank.centerred + mean.kina + kina.centerred + mean.connection + connections.centerred + mean.DSI.gregariousness + gregariousness.centerred+ (1|focal.id), data=f.predictors.analysis)

vif(predictors)

summary(predictors)
drop1(predictors,test="Chisq")

qqPlot(residuals(predictors))
plot(residuals(predictors)~fitted(predictors))
hist(predictors@u)

#logit transformation? 
predictors.logit<-lmer(logit ~ mother.alive + mean.age + age.centerred + mean.rank + rank.centerred + mean.kina + kina.centerred + mean.connection + connections.centerred + mean.DSI.gregariousness + gregariousness.centerred+ (1|focal.id), data=f.predictors.analysis)

vif(predictors.logit)

summary(predictors.logit)
drop1(predictors.logit,test="Chisq")

qqPlot(residuals(predictors.logit))
plot(residuals(predictors.logit)~fitted(predictors.logit))
hist(predictors.logit@u)

lrtest(predictors,predictors.logit)
#linear model is much better?

#proportional binomial model

predictors.pb <- glmer(top3.nonkin/(top3.total)~mother.alive + mean.age + age.centerred + mean.rank + rank.centerred + mean.kina + kina.centerred + mean.connection + connections.centerred + mean.DSI.gregariousness + gregariousness.centerred + (1|focal.id),weights=top3.total,family=binomial,data=f.predictors.analysis)

vif(predictors.pb)

summary(predictors.pb)
drop1(predictors.pb,test="Chisq")

qqPlot(residuals(predictors.pb))
plot(residuals(predictors.pb)~fitted(predictors.pb))
hist(predictors.pb@u)

lrtest (predictors, predictors.pb) 
#linear model > proportional binomial > logit transformation

age.kina <- f.predictors.analysis %>% 
  group_by(focal.id) %>%
  dplyr::select(focal.id,mean.age,mean.kina) %>%
  unique() %>%
  ungroup()

cor.test(age.kina$mean.age,age.kina$mean.kina)

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

f.desc<-all.desc %>%
  subset(group == "f")
xyplot(focal.percofsex.dominanted~focal.age|focal.id,data=f.desc,pch=16,cex=0.6, type=c("p","r"))
xyplot(focal.percofsex.dominanted~focal.kin.available|focal.id,data=f.desc,pch=16,cex=0.6, type=c("p","r"))
xyplot(focal.age~focal.kin.available|focal.id,data=f.desc,pch=16,cex=0.6, type=c("p","r"))

#no obvious strong correlation of rank with age or nomber of kin available 

f.desc <- f.desc %>%
  mutate(logitr=log((focal.percofsex.dominanted+0.1)/(100.1-focal.percofsex.dominanted)))
hist(f.desc$logitr)

f.desc <-f.desc %>%
  group_by(focal.id) %>%
  mutate(mean.age = mean(focal.age)) %>%
  mutate(age.centerred = focal.age - mean.age) %>%
  mutate(mean.kina = mean(focal.kin.available)) %>%
  mutate(kina.centerred = focal.kin.available - mean.kina) %>%
  ungroup()

rank.age.kin <- lmer (logitr ~ mean.age + age.centerred + mean.kina + kina.centerred + (1 | focal.id) , data = f.desc)
summary(rank.age.kin)
drop1(rank.age.kin,test = "Chisq")
vif(rank.age.kin)
qqPlot(residuals(rank.age.kin))
plot(residuals(rank.age.kin)~fitted(rank.age.kin)) 
#within subject, rank is negatively correlated with age, and positively with number of kin available

a <- flong.yearly %>% 
  subset(binary == "non-kin" & top3 == T & years.in.top3 ==1)

unique(a$dyad)


