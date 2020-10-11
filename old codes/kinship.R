#load the packages
rm(list=ls())
library(dplyr)
library(tidyr)
library(kinship2)
setwd("~/Desktop/Non-kin cooperation data/F/txt files/kinship")

#load F2010 kinship data
f2010<-read.csv("f2010.txt",header=T)
f2010$year<-2010

f2011<-read.csv("f2011.txt",header=T)
f2011$year<-2011

f2012<-read.csv("f2012.txt",header=T)
f2012$year<-2012

f2013<-read.csv("f2013.txt",header=T)
f2013$year<-2013

f2014<-read.csv("f2014.txt",header=T)
f2014$year<-2014

f2015<-read.csv("f2015.txt",header=T)
f2015$year<-2015

f2016<-read.csv("f2016.txt",header=T)
f2016$year<-2016

f2017<-read.csv("f2017.txt",header=T)
f2017$year<-2017

#preparation of the data
fkinship<-rbind(f2010,f2011,f2012,f2013,f2014,f2015,f2016,f2017)
fkinship<-fkinship %>% select (id,sex,year.of.birth,mother,father,behavioral.mother,group.at.birth)

#delete duplicates from different years
fkinship<-unique(fkinship)

#add the first generation to the id
father0<-data.frame(fkinship$father) 
colnames(father0)<-("id")
father0$sex<-"M"
mother0<-data.frame(fkinship$behavioral.mother)
colnames(mother0)<-("id")
mother0$sex<-"F"
fkinship<-bind_rows(fkinship,father0,mother0)
fkinship1<-fkinship %>% drop_na(id) 
fkinship2 <- distinct(fkinship1,id,.keep_all = T)

#if a monkey has only one parent info, make the other parent info unavailable too. comes as a solution to the pedigree function that requires that either both or none of the two parents are known. 
fkinship2$behavioral.mother[is.na(fkinship2$father)]<-NA
fkinship2$father[is.na(fkinship2$behavioral.mother)]<-NA

#building pedigree and kinship network
fped<-with(fkinship2,pedigree(id,father,behavioral.mother,sex))
kinship<-with(fkinship2,kinship(id,father,behavioral.mother,sex))
plot(fped, id = fped$id, status = fped$status, affected =
       fped$affected, cex = 1, col = 1, symbolsize = 1, branch = 0.6, packed = TRUE, align = c(1.5,2), width = 8, density = c(-1, 35, 65, 20), mar = c(4.1, 1, 4.1, 1), angle = c(90, 65, 40, 0), keep.par = FALSE,  pconnect= .5)

#not sure what I am doing here...
fped.shrink<-pedigree.shrink(fped,avail=(fkinship2$sex=="F"),affected=NULL, seed=NULL, maxBits=200)
fped.shrink
plot(fped.shrink,cex = 1, col = 1, symbolsize = 1, branch = 0.6, packed = TRUE, align = c(1.5,2), width = 8, density = c(-1, 35, 65, 20), mar = c(4.1, 1, 4.1, 1), angle = c(90, 65, 40, 0), keep.par = FALSE,  pconnect= .5)
View(kinship)

?plot
