rm(list=ls())

library(dplyr)
library(tidyr)
library(kinship2)
library(reshape2)

setwd("~/Desktop/Non-kin cooperation data/F")
fpedigree<-read.csv("~/Desktop/Non-kin cooperation data/pedigree.csv",header=T)

#find out where behaviroal mother is not the biological mother
diffmom<-as.character(fpedigree$DAM) %in% c("",as.character(fpedigree$BEHAVIORAL.MOM))
print(fpedigree[diffmom==F,])

fpedigree<- fpedigree %>% 
  select(ID,SEX,SIRE,BEHAVIORAL.MOM)

#10 individuals have same ID: 0.00E+00
fpedigree<-fpedigree[fpedigree$ID!="0.00E+00",]

fr<-with(fpedigree,kinship(ID,SIRE,BEHAVIORAL.MOM,SEX))

f2010r<-read.csv("f2010.txt",header=T)
f2010r<-fr[as.character(f2010r$id),as.character(f2010r$id)]
f2010r<-as.data.frame(as.table(f2010r))
colnames(f2010r)<-c("focal.monkey","partner","r")

#read in F 2010 grooming data
f2010g<-read.csv("f2010g.txt",header=T)

#extrait the focal animal from observation session codes
f2010g<-cbind(f2010g,colsplit(f2010g$observation.session, "\\.", names = c("month", "day","year","focal.monkey","No."))) 
f2010g$duration[is.na(f2010g$duration)]<-0

#calculate the duration of all interactions of one dyad 
f2010g <- f2010g %>% 
  mutate_each(funs(toupper), "focal.monkey")  %>%
  group_by(groom.giver, groom.reciever,focal.monkey) %>% 
  summarise_at(vars(duration), sum) %>%
  ungroup()

#read in the total hours of focal followed

f2010g <- f2010g %>%
  mutate(partner=ifelse(as.character(focal.monkey)==as.character(groom.reciever),as.character(groom.giver),as.character(groom.reciever)))

f2010<-read.csv("f2010.txt",header=T)
colnames(f2010)[1]<-"focal.monkey"

#merge them
f2010g<-left_join(f2010r,f2010g) 
f2010g<-left_join(f2010g,f2010)
colnames(f2010g)<-c("focal.monkey","partner","r","groom.giver","groom.receiver","groom.duration","focal.sex","focal.yob","focal.age","focal.mother","focal.father","focal.behavmother","focal.gab","focal.rank","focal.ordinal.rank","focal.dom","focal.idcode","focal.hrsfocalfollowed")

colnames(f2010)[1]<-"partner"
f2010g<-left_join(f2010g,f2010)

colnames(f2010g)[19:30]<-c("partner.sex","partner.yob","partner.age","partner.mother","partner.father","partner.behavmother","partner.gab","partner.rank","partner.ordinal.rank","partner.dom","partner.idcode","partner.hrsfocalfollowed")
f2010g$groom.duration[is.na(f2010g$groom.duration)]<-0

f2010g1<-f2010g %>% 
  group_by(focal.monkey,partner) %>% 
  summarise(total.grooming=sum(groom.duration)) %>%
  ungroup()

f2010g<-left_join(f2010g,f2010g1)
  
#calculate the DSI (grooming part) of the focals with their partners
f2010g$DSIg<-f2010g$total.grooming/3600/f2010g$focal.hrsfocalfollowed

f2010p<-read.csv("f2010p.txt",header=T)
f2010p$observation.scan<-paste(f2010p$observation.name,f2010p$scan.number,sep=".")

f2010p1<-f2010p %>%
  group_by(focal.monkey) %>%
  summarize(focal.total.scan=n()) %>%
  ungroup ()

f2010p<-left_join(f2010p,f2010p1)

f2010p<-separate_rows(f2010p, in.proximity, convert = TRUE)


f2010p2<- f2010p %>% 
  group_by(focal.monkey,in.proximity) %>%
  summarize(dyad.proximity=n()) %>%
  ungroup ()

f2010p<-left_join(f2010p,f2010p2) %>%
  select(focal.monkey,in.proximity,focal.total.scan,dyad.proximity) %>%
  unique() %>%
  mutate(DSIp=dyad.proximity/focal.total.scan) 

colnames(f2010p)[2]<-"partner"

f2010DSI<-full_join(f2010p,f2010g)

f2010DSI<- f2010DSI[f2010DSI$partner != f2010DSI$focal.monkey,]

f2010DSI$DSIp[is.na(f2010DSI$DSIp)]<- 0

f2010DSI <- f2010DSI %>% 
  mutate(DSI=DSIg+DSIp) 

f2010DSI[] <- lapply(f2010DSI, as.character)
f2010DSI$focal.behavmother[is.na(f2010DSI$focal.behavmother)]<-"0"
f2010DSI$focal.father[is.na(f2010DSI$focal.father)]<-"0"
f2010DSI$partner.behavmother[is.na(f2010DSI$partner.behavmother)]<-"0"
f2010DSI$partner.father[is.na(f2010DSI$partner.father)]<-"0"

f2010DSI<-f2010DSI %>% 
  mutate(relationship=ifelse(focal.monkey == partner.behavmother, "mother-child",
                             ifelse(focal.monkey == partner.father, "father-child", 
                                    ifelse(partner == focal.behavmother, "child-mother", 
                                           ifelse(partner == focal.father, "child-father",
                                                  ifelse(focal.behavmother == partner.behavmother, "maternal siblings", 
                                                         ifelse(focal.father == partner.father, "paternal siblings","others")))))))


write.csv(f2010old,"f2010DSI.csv")


