rm(list=ls())

library(gtools)
library(dplyr)
library(tidyr)
library(kinship2)
library(reshape2)
library(stringr)

#pedigree
setwd("~/Desktop/Non-kin cooperation data/F")
pedigree<-read.csv("~/Desktop/Non-kin cooperation data/pedigree.csv",header=T)

diffmom<-as.character(pedigree$DAM) %in% c("",as.character(pedigree$BEHAVIORAL.MOM))
print(pedigree[diffmom==F,])

pedigree<- pedigree %>% 
  select(ID,SEX,SIRE,BEHAVIORAL.MOM)

pedigree<-pedigree[pedigree$ID!="0.00E+00",]

pedigree<-with(pedigree,kinship(ID,SIRE,BEHAVIORAL.MOM,SEX))

#f2010
f2010<-read.csv("f2010.txt",header=T) %>% 
  filter(sex=="F") 

f2010pedigree<-as.data.frame(as.table(pedigree[as.character(f2010$id),as.character(f2010$id)])) %>%
  setNames(c("focal.id","partner.id","r")) %>%
  filter(focal.id!=partner.id) %>%
  mutate(r=r*2)

f2010dummy<-f2010 %>%
  rename_all(function(x) paste0("partner.",x))

f2010<-f2010 %>% 
  rename_all(function(x) paste0("focal.",x))

f2010<-merge(f2010dummy,f2010) %>%
  filter(focal.id!=partner.id)

f2010<-full_join(f2010,f2010pedigree,by = c("partner.id", "focal.id"))
f2010<- lapply(f2010, as.character)

sapply(f2010, function(x) sum(is.na(x)))

f2010<-data.frame(f2010,stringsAsFactors=FALSE)
f2010[is.na(f2010)] <- sample(as.character(seq(0, 1, 0.00001)),sum(is.na(f2010)),replace=T)

f2010 <- f2010 %>% 
  mutate(relationship=ifelse(focal.id == partner.id, "self",
        ifelse(r == 0, "non-kin",
           ifelse(focal.id == partner.behavioral.mother, "mother-child",
                             ifelse(focal.id == partner.father, "father-child", 
                                    ifelse(partner.id == focal.behavioral.mother, "child-mother", 
                                           ifelse(partner.id == focal.father, "child-father",
                                                  ifelse(focal.behavioral.mother == partner.behavioral.mother, "maternal siblings", 
                                                         ifelse(focal.father == partner.father, "paternal siblings","other kin")))))))))

view.other.kin<-subset(f2010,relationship=="other kin") %>%
  select(focal.id,focal.father,focal.behavioral.mother,partner.id,partner.father,partner.behavioral.mother,r,relationship)

#grooming
f2010g<-read.csv("f2010g.txt",header=T)

sapply(f2010g, function(x) sum(is.na(x)))

f2010g<-replace_na(f2010g,list(duration=0))
#replace NA duration with zero

f2010g <- f2010g %>% 
  group_by(groom.giver, groom.reciever) %>% 
  summarise_at(vars(duration), sum) %>%
  ungroup() %>%
  subset(groom.giver %in% f2010$focal.id & groom.reciever %in% f2010$focal.id)

f2010gdummy <-f2010g %>% 
  setNames(c("focal.id","partner.id","groom.giving"))

f2010<-full_join(f2010,f2010gdummy)

f2010gdummy <-f2010g %>% 
  setNames(c("partner.id","focal.id","groom.receiving"))

f2010<-full_join(f2010,f2010gdummy)

f2010[is.na(f2010)]<-0

f2010<-f2010 %>%
  mutate(grooming.rate = (groom.giving+groom.receiving)/(as.numeric(focal.hrs.focalfollowed)+as.numeric(partner.hrs.focalfollowed))/3600) %>%
  mutate(grooming.rate.over.mean=grooming.rate/mean(grooming.rate))

#proximity
f2010p<-read.csv("f2010p.txt",header=T) %>%
  filter(focal.monkey %in% f2010$focal.id) 

f2010pdummy<-f2010p %>%
  group_by(focal.monkey) %>%
  summarize(focal.total.scan=n()) %>%
  ungroup ()

f2010p<-left_join(f2010p,f2010pdummy)

f2010p<- f2010p %>%
  separate_rows(in.proximity, convert = TRUE) %>%
  filter(focal.monkey != in.proximity) %>%
  filter(in.proximity %in% f2010$focal.id)

 f2010pdummy<- f2010p %>% 
  group_by(focal.monkey,in.proximity) %>%
  mutate(no.scan=n()) %>%
  select(4,5,9,10) %>%
  ungroup () %>%
  unique()
 
 f2010pdummy1<-f2010pdummy %>%
   select(1,3) %>%
   setNames(c("focal.id","focal.total.scan")) %>%
   unique()
 
 f2010<-left_join(f2010,f2010pdummy1)
 
 f2010pdummy1<-f2010pdummy1 %>%
   setNames(c("partner.id","partner.total.scan"))
 
 f2010<-left_join(f2010,f2010pdummy1)
 
 f2010pdummy <-f2010pdummy %>% 
   select(1,2,4) %>%
   setNames(c("focal.id","partner.id","focal.no.scan.with.partner"))
 
 f2010<-left_join(f2010,f2010pdummy)
 
 f2010pdummy <-f2010pdummy %>% 
   setNames(c("partner.id","focal.id","partner.no.scan.with.focal"))
 
 f2010<-left_join(f2010,f2010pdummy)

 f2010[is.na(f2010)]<-0
 
 f2010<-f2010 %>%
   mutate(proximity.rate = (focal.no.scan.with.partner+partner.no.scan.with.focal)/(focal.total.scan+partner.total.scan)) %>%
   mutate(proximity.rate.over.mean=proximity.rate/mean(proximity.rate))
 
 sapply(f2010, function(x) sum(is.na(x)))

 f2010 <- f2010 %>%
   mutate(DSI=(grooming.rate.over.mean+proximity.rate.over.mean)/2)
 
 write.csv(f2010,"f2010DSI.csv")
 