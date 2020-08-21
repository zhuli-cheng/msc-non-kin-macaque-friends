rm(list = ls())

library(lattice)

filename <-
  list.files(
    "~/Desktop/Non-kin cooperation data/wrangled data",
    pattern = "*.csv",
    full.names = TRUE
  )

allDSI <- lapply(filename, function(x) {
  read_csv(
    x,
    col_types = cols(
      focal.sex = "f",
      partner.sex = "f",
      focal.group.at.birth = "f",
      partner.group.at.birth = "f"
    )
  )
})

F2010<-read.csv("~/Desktop/Non-kin cooperation data/wrangled data/f2010DSI.csv",header=T)
F2010a <- F2010 %>%
  group_by(focal.id) %>%
  mutate(order.of.partner = ifelse (DSI==0,0,rank(-DSI,ties.method= "min"))) %>%
  mutate(top3 = order.of.partner %in% 1:3) %>%
  mutate(no.of.connection = sum (DSI!=0)) %>%
  mutate(no.of.kin.available = sum (binary == "kin")) %>%
  mutate(per.kin.in.top3 = sum(binary == "kin" & top3 == T)/sum (top3 == T)) %>%
  mutate(per.nonkin.in.top3 = sum(binary == "non-kin" & top3 == T)/sum (top3 == T)) 
    
  
#percentage of time spent with non kin 

View (F2010a[F2010a$binary=="kin","relationship" $ "no.of.kin.available"])

mean (F2010a$per.kin.in.top3 >= 0.5)

#number of kin available
#number of non-kin top3 partners
#what are the chance for kin to be top3 and non-kin top3, on the level of how many kin available 

attach(F2010a)
boxplot(as.numeric(per.nonkin.in.top3)~as.numeric(no.of.kin.available))
boxplot(as.numeric(per.nonkin.in.top3)~as.numeric(no.of.connection))

#no matter how many kin available (<=8), there were always non-kin top partner
