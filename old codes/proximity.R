rm(list=ls())
setwd("~/Desktop/Non-kin cooperation data/F/txt files/proximity")
library(dplyr)
library(reshape2)

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

f2010DSI<-full_join(f2010p,f2010g,by=())
