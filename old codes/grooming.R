
rm(list=ls())
library(dplyr)
library(reshape2)

setwd("~/Desktop/Non-kin cooperation data/F/txt files/grooming")

#read in F 2010 grooming data
f2010g<-read.csv("f2010g.txt",header=T)

#extrait the focal animal from observation session codes
f2010g<-cbind(f2010g,colsplit(f2010g$observation.session, "\\.", names = c("month", "day","year","focal.monkey","No."))) 

#calculate the duration of all interations of one dyad 
f2010g <- f2010g %>% 
  mutate_each(funs(toupper), "focal.monkey")  %>%
  group_by(groom.giver, groom.reciever,focal.monkey) %>% 
  summarise_at(vars(duration), sum) 

#read in the total hours of focal followed
f2010<-read.csv("~/Desktop/Non-kin cooperation data/F/txt files/kinship/f2010.txt",header=T)
colnames(f2010)[1]<-"focal.monkey"

#merge them
f2010g<-left_join(f2010g,f2010) 

#select the relevant columns
f2010g<-f2010g %>% 
  select (groom.giver,groom.reciever,duration,focal.monkey,hrs.focalfollowed,sex,age,percofsex.dominanted)

#calculate the DSI (grooming part) of the focals with their partners
f2010g$DSIg<-f2010g$duration/3600/f2010g$hrs.focalfollowed

#problem: NA in duration of one observation session makes the duration of interaction of the dyad NA. 
#solution: make the NA = zero? 


