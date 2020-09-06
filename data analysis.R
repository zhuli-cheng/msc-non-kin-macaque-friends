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

all <- all %>%
  group_by(focal.id) %>%
  mutate(mean.DSI = mean(DSI)) %>%
  ungroup()

#0. general 
ggplot(all, aes(x=DSI)) +
  geom_density()

ggplot(all, aes(x=DSI+0.001)) +
  geom_density()+
  scale_x_log10(labels = function(x) format(x, scientific = F))
#most pairs had no interactions 

ggplot(all, aes(x=mean.DSI+0.001)) +
  geom_density()+
  scale_x_log10(labels = function(x) format(x, scientific = F)) 
#females differ in gregariousness, some had no interaction at all



#1.1 DSI ~ kinship

#1.1.1 DSI ~ binary
ggplot(all, aes(x=DSI+0.001, color=binary)) +
  geom_density() +
  scale_x_log10(labels = function(x) format(x, scientific = F))
#kin dyads are more likely to have interactions and higher DSI, but non-kin dyad can also have very high DSI

ggplot(all, aes(x=grooming.rate.over.mean+0.001, color=binary)) +
  geom_density() +
  scale_x_log10(labels = function(x) format(x, scientific = F))

ggplot(all, aes(x=proximity.rate.over.mean+0.001, color=binary)) +
  geom_density()+
  scale_x_log10(labels = function(x) format(x, scientific = F))
#grooming is less likely to happen than proximity 

#1.1.2 DSI ~ r
ggplot(all,aes(x=r,y=DSI+0.001)) +
  geom_point(alpha=0.5, aes(color=group)) + 
  scale_y_log10(labels = function(x) format(x, scientific = F)) 


#DSI is positively correlated (and linearly when logged) with r

DSI.r.p<-glm(DSI ~ r, data=all,family = poisson)
DSI.r.nb <-glm.nb(DSI ~ r, data=all)

DSI.r.p.m <- glmer (DSI ~ r + (1|focal.id), data=all, nAGQ=0,family=poisson)
DSI.r.nb.m <- glmer.nb (DSI ~ r + (1|focal.id), data=all, nAGQ=0)


#because of over dispersion, perhaps use negative binomial distribution? (quasi poisson cannot be used on mixed models)
#random slope? DSI.r.nb.m1 <- glmer.nb (DSI ~ r + (r|focal.id), data=all, nAGQ=0)

lrtest(DSI.r.p,DSI.r.nb)
lrtest(DSI.r.p.m,DSI.r.nb.m)
#nb is better than poisson

lrtest(DSI.r.nb,DSI.r.nb.m)
#mixed model is better 

drop1(DSI.r.nb.m,test="Chisq")
#r improves model fit

View(summary(DSI.r.nb.m))
summary(DSI.r.nb.m)[["coefficients"]]

fixed <- fixef(DSI.r.nb.m)
confintfixed <- confint(DSI.r.nb.m, parm = "beta_", method = "Wald") # Beware: The Wald method is less accurate but much, much faster.

# The exponentiated coefficients are also known as Incidence Rate Ratios (IRR)
IRR <- exp(cbind(fixed, confintfixed))
IRR
# with r increased by 1, DSI increased by 3715

#check assumptions: normality of intercepts and slopes; log of counts is linearly correlated to the predictor; dispersion
dispersion_glmer(DSI.r.p.m)
dispersion_glmer(DSI.r.nb.m)

hist(intercep.nb<-coef(DSI.r.nb.m)$focal.id[,1])


#1.1.3 DSI ~ relationship
ggplot(all, aes(x=DSI+0.001, color=relationship)) +
  geom_density() +
  scale_x_log10(labels = function(x) format(x, scientific = F))

#1.2 top3 ~ kinship

#1.2.1 top3 ~ binary
ggplot(all) + 
  geom_bar(aes(x = binary, fill = top3)) +
  facet_wrap(~group,scales="free_y")

top3<-all %>%
  subset(top3==T) %>%
  mutate(order.of.partner=as.factor(order.of.partner))

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
}

ggplot(data = top3, aes(x=order.of.partner, y=log(DSI+0.001)) )  +
  geom_boxplot(aes(fill=binary)) +
  geom_point(aes(y=log(DSI+0.001), group=binary), position = position_dodge(width=0.75))+ 
  facet_wrap( ~ order.of.partner, scales="free") + 
  xlab("Top Partners") + ylab("DSI") + ggtitle("All Group Years")+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median, aes(group=binary),
                        position = position_dodge(width = 0.75),hjust = 1)+
  guides(fill=guide_legend(title="Dyad"))

top3.binary <- glmer(top3 ~ binary + (1|group) + (1|year), family = binomial, data=all)
top3.binary1 <- glm(top3 ~ binary , family = binomial, data=all)
lrtest(top3.binary,top3.binary1)
#mixed model is slightly better

summary(top3.binary)
drop1(top3.binary)
#binary is significant predictor 

fixed <- fixef(top3.binary)
confintfixed <- confint(top3.binary, parm = "beta_", method = "Wald")
IRR <- exp(cbind(fixed, confintfixed))
IRR
#non-kin vs kin: odds of being top3 decreased to its 11% 

#check assumptions: no missing combinations;outliers; random intercepts 
chisq.test(all$top3,all$binary)$expected
dispersion_glmer(top3.binary)
hist(intercept<-coef(top3.binary)$group[,1])
hist(intercept<-coef(top3.binary)$year[,1])

# 1.2.2 top3 ~ r

ggplot(all) + 
  geom_count(aes(x = r, y = top3)) +
  facet_wrap(~group,scales="free_y")

top3.r <- glmer(top3 ~ r + (1|group) + (1|year), family = binomial, data=all)
top3.r1 <- glm(top3 ~ r , family = binomial, data=all)
lrtest(top3.r,top3.r1)
#mixed model is slightly better

summary(top3.r)
drop1(top3.r)
#r is significant predictor 

fixed <- fixef(top3.r)
confintfixed <- confint(top3.r, parm = "beta_", method = "Wald")
IRR <- exp(cbind(fixed, confintfixed))
IRR
#r increase by 1: odds of being top3 increase by 9717!

#check assumptions: linearity in the logit;outliers; random intercepts 
dispersion_glmer(top3.r)
hist(intercept<-coef(top3.r)$group[,1])
hist(intercept<-coef(top3.r)$year[,1])

top3.r.lin<-glmer(top3 ~ r + r*log(r+0.001)+(1|group) + (1|year), family = binomial, data=all)
summary(top3.r.lin)
#linearity in the logit is violated!

#1.2.3 top3 ~ relationships
ggplot(all) + 
  geom_bar(aes(x = relationship, fill = top3)) +
  facet_wrap(~group,scales="free_y")

#2. longitudinal
#sd/mean? 

#longitudinal analysis 
alllong <- all %>%
  group_by(focal.id,partner.id) %>%
  mutate(dyad.total.years = n()) %>%
  mutate(mean.dyadic.DSI = mean(DSI)) %>%
  mutate(dyadic.DSI.sd = sd(DSI)) %>% 
  mutate(delta.DSI = c(NA, diff(DSI))) %>%
  mutate(delta.order = c(NA,diff(order.of.partner))) %>%
  ungroup() %>%
  select(groupyear:DSI,mean.DSI:delta.DSI,focal.connections:order.of.partner,delta.order,top3:partner.percofsex.dominanted)

flong <- alllong %>%
  subset ( group == "f" & dyad.total.years > 1 & meanDSI.over.years > 1 )

sapply(flong, function(x) sum(is.na(x)))

mean(flong$meanDSI.over.years[flong$binary=="kin"]) #14.71
mean(flong$meanDSI.over.years[flong$binary=="non-kin"]) #3.08

mean(flong$sdDSI.over.years[flong$binary=="kin"]) #9.78
mean(flong$sdDSI.over.years[flong$binary=="non-kin"]) #4.20

mean(flong$delta.DSI[flong$binary=="kin" & !is.na(flong$delta.DSI)]) #-0.22
mean(flong$delta.DSI[flong$binary=="non-kin"  & !is.na(flong$delta.DSI)]) #-0.21

mean(abs(flong$delta.DSI[flong$binary=="kin" & !is.na(flong$delta.DSI)])) #10.73
mean(abs(flong$delta.DSI[flong$binary=="non-kin"  & !is.na(flong$delta.DSI)])) #4.50

mean(flong$delta.order[flong$binary=="kin" & !is.na(flong$delta.order)]) #-0.16
mean(flong$delta.order[flong$binary=="non-kin"  & !is.na(flong$delta.order)])#-0.64

plot(data = flong, delta.DSI~as.factor(binary))
plot(data = flong, delta.order~as.factor(binary))

#death of mother
deadmother<-read.csv("~/Desktop/Non-kin cooperation data/pedigree.csv",header=T,na.strings = c("", "NA")) %>%
  select("ID","DOD","REMOVE.DATE") %>%
  subset(!is.na(DOD) | !is.na(REMOVE.DATE))%>%
  mutate(DOD = str_replace(DOD,".*/.*/","")) %>%
  mutate(REMOVE.DATE = str_replace(REMOVE.DATE,".*/.*/","") ) %>%
  gather(dead.or.removed,year,DOD:REMOVE.DATE,na.rm=T) %>%
  mutate(dead.or.removed = ifelse(dead.or.removed == "DOD","dead","removed")) %>%
  setNames(c("focal.behavioral.mother","mother.dead.or.removed","year.mother.left")) %>%
  mutate(year.mother.left = as.numeric(year.mother.left))

fdeadmother <- alllong %>%
  subset(group == "f")

fdeadmother<-left_join(fdeadmother,deadmother, by = c("focal.behavioral.mother"))

fdeadmother <- fdeadmother %>%
  mutate(mother.alive = ifelse(is.na(year.mother.left),"mother alive",ifelse(year.mother.left == year, "mother leaving", ifelse (year < year.mother.left, "before mother left", ifelse (year > year.mother.left, "mother left", NA)))))

fdmanalysis<- fdeadmother %>%
  select (focal.id,focal.behavioral.mother,year,per.nonkin.in.top3,mother.alive,focal.connections,focal.kin.available,focal.age,focal.ordinal.rank,focal.percofsex.dominanted,focal.rank) %>%
  unique()

ggplot(fdmanalysis, aes(year, per.nonkin.in.top3)) +
  geom_point(aes(color = mother.alive)) + 
  facet_wrap(~ focal.id) 

plot(per.nonkin.in.top3 ~ as.factor(mother.alive), data=fdmanalysis)


#check the assumptions: multicollinearity,random effects, normality of residual and data, logistic fit, over-dispersion?, outliers







#3. predictors of how many non-kin top partners 
predictors<-glmer(per.nonkin.in.top3 ~ focal.age +focal.perofsex.dominated + focal.kin.available + focal.connections )
#compare means
mean(all$focal.age) #12.28
mean(nonkintop1$focal.age) #11.99

mean(all$focal.kin.available) #4.05
mean(nonkintop1$focal.kin.available) #3.12

mean(all$focal.connections) #10.07
mean(nonkintop1$focal.connections) #10.96



plot(as.numeric(all$per.nonkin.in.top3)~as.factor(all$focal.kin.available))
plot(as.numeric(all$per.nonkin.in.top3)~as.factor(all$focal.connections))


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
