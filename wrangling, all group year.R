rm(list = ls())

library(gtools)
library(dplyr)
library(tidyr)
library(kinship2)
library(reshape2)
library(stringr)
library(readr)

#pedigree
setwd("~/Desktop/Non-kin cooperation data/txt data")
pedigree <-
  read.csv("~/Desktop/Non-kin cooperation data/pedigree.csv", header = T)

diffmom <-
  as.character(pedigree$DAM) %in% c("", as.character(pedigree$BEHAVIORAL.MOM))
print(pedigree[diffmom == F, ])

pedigree <- pedigree %>%
  select(ID, SEX, SIRE, BEHAVIORAL.MOM)

pedigree <- with(pedigree, kinship(ID, SIRE, BEHAVIORAL.MOM, SEX))

#data loading
filenames <-
  list.files(
    "~/Desktop/Non-kin cooperation data/txt data",
    pattern = "*.txt",
    full.names = TRUE
  )



alldata <- lapply(filenames, function(x) {
  read_csv(x, col_types = cols(sex = "f"))
})

names(alldata) <- gsub(".*/(.*)\\..*", "\\1", filenames)

alldata <- by(
  seq_along(alldata),
  cut(seq_along(alldata), 19),
  FUN = function(x)
    alldata[x]
)

groupyears <-
  c(
    "f2010",
    "f2011",
    "f2012",
    "f2013",
    "f2014",
    "f2015",
    "f2016",
    "f2017",
    "hh2014",
    "hh2016",
    "kk2013",
    "kk2015",
    "kk2017",
    "r2015",
    "r2016",
    "s2011",
    "v2015",
    "v2016",
    "v2017"
  )

names(alldata) <- groupyears

#wrangling
wrangling <- function(i) {
  gy <- as.data.frame.list(alldata[[i]][[1]])
  
  colnames(gy) <- names(alldata[[i]][[1]])
  
  gy <- gy %>%
    filter(sex == "F"  & age >= 6)
  
  gypedigree <-
    as.data.frame(as.table(pedigree[as.character(gy$id), as.character(gy$id)])) %>%
    setNames(c("focal.id", "partner.id", "r"))  %>%
    filter(focal.id != partner.id) %>%
    mutate(r = r * 2)
  
  gydummy <- gy %>%
    rename_all(function(x)
      paste0("partner.", x))
  
  gy <- gy %>%
    rename_all(function(x)
      paste0("focal.", x))
  
  gy <- merge(gydummy, gy) %>%
    filter(focal.id != partner.id)
  
  gy <- full_join(gy, gypedigree, by = c("partner.id", "focal.id"))
  
  gy <- lapply(gy, as.character)
  
  sapply(gy, function(x)
    sum(is.na(x)))
  
  gy <- data.frame(gy, stringsAsFactors = FALSE)
  gy[is.na(gy)] <-
    sample(as.character(seq(0, 1, 0.00001)), sum(is.na(gy)), replace = T)
  
  gy <- gy %>%
    mutate(binary = ifelse(r >= 0.125, "kin", "non-kin")) %>%
    mutate(relationship = ifelse(
      r == 0,
      "non-kin",
      ifelse(
        focal.id == partner.behavioral.mother,
        "mother-child",
        ifelse(
          focal.id == partner.father,
          "father-child",
          ifelse(
            partner.id == focal.behavioral.mother,
            "child-mother",
            ifelse(
              partner.id == focal.father,
              "child-father",
              ifelse(
                focal.behavioral.mother == partner.behavioral.mother,
                "maternal siblings",
                ifelse(
                  focal.father == partner.father,
                  "paternal siblings",
                  "other kin"
                )
              )
            )
          )
        )
      )
    ))
  
  #grooming
  
  gyg <- as.data.frame.list(alldata[[i]][[2]])
  
  colnames(gyg) <- names(alldata[[i]][[2]])
  
  sapply(gyg, function(x)
    sum(is.na(x)))
  
  gyg <- replace_na(gyg, list(duration = 0))
  #to decide: whether replace NA duration with zero?
  
  gyg <- gyg %>%
    group_by(groom.giver, groom.reciever) %>%
    summarise_at(vars(duration), sum) %>%
    ungroup() %>%
    subset(
      groom.giver %in% gy$focal.id &
        groom.reciever %in% gy$focal.id &
        as.character(groom.giver) != as.character(groom.reciever)
    )
  
  gygdummy <- gyg %>%
    setNames(c("focal.id", "partner.id", "groom.giving"))
  
  gy <- full_join(gy, gygdummy, by = c("partner.id", "focal.id"))
  
  gygdummy <- gyg %>%
    setNames(c("partner.id", "focal.id", "groom.receiving"))
  
  gy <- full_join(gy, gygdummy, by = c("partner.id", "focal.id"))
  
  gy[is.na(gy)] <- 0
  
  gy <- gy %>%
    mutate(grooming.rate = (groom.giving + groom.receiving) / (
      as.numeric(focal.hrs.focalfollowed) + as.numeric(partner.hrs.focalfollowed)
    ) / 3600) %>%
    mutate(grooming.rate.over.mean = grooming.rate / mean(grooming.rate))
  
  #proximity
  gyp <- as.data.frame.list(alldata[[i]][[3]])
  
  colnames(gyp) <- names(alldata[[i]][[3]])
  
  gyp <- gyp %>%
    filter(focal.monkey %in% gy$focal.id)
  
  gypdummy <- gyp %>%
    group_by(focal.monkey) %>%
    summarize(focal.total.scan = n()) %>%
    ungroup ()
  
  gypdummy1 <- gypdummy %>%
    select(c("focal.monkey", "focal.total.scan")) %>%
    setNames(c("focal.id", "focal.total.scan")) %>%
    unique()
  
  gy <- left_join(gy, gypdummy1, by = "focal.id")
  
  gypdummy1 <- gypdummy1 %>%
    setNames(c("partner.id", "partner.total.scan"))
  
  gy <- left_join(gy, gypdummy1, by = "partner.id")
  
  gyp <- gyp %>%
    separate_rows(in.proximity, convert = TRUE) %>%
    filter(focal.monkey != in.proximity) %>%
    filter(in.proximity %in% gy$focal.id)
  
  gypdummy <- gyp %>%
    group_by(focal.monkey, in.proximity) %>%
    mutate(no.scan = n()) %>%
    select(4, 5, 9) %>%
    ungroup () %>%
    unique()
  
  gypdummy <- gypdummy %>%
    setNames(c("focal.id", "partner.id", "focal.no.scan.with.partner"))
  
  gy <- left_join(gy, gypdummy, by = c("partner.id", "focal.id"))
  
  gypdummy <- gypdummy %>%
    setNames(c("partner.id", "focal.id", "partner.no.scan.with.focal"))
  
  gy <- left_join(gy, gypdummy, by = c("partner.id", "focal.id"))
  
  gy[is.na(gy)] <- 0
  
  gy <- gy %>%
    mutate(proximity.rate = ifelse ((focal.total.scan + partner.total.scan ==
                                       0),
                                    0,
                                    (focal.no.scan.with.partner + partner.no.scan.with.focal) / (focal.total.scan +
                                                                                                   partner.total.scan)
    )) %>%
    mutate(proximity.rate.over.mean = proximity.rate / mean(proximity.rate))
  
  sapply(gy, function(x)
    sum(is.na(x)))
  
  gy <- gy %>%
    mutate(DSI = (grooming.rate.over.mean + proximity.rate.over.mean) /
             2)
  
  gy <- gy %>%
    group_by(focal.id) %>%
    mutate(focal.connections = sum (DSI!=0)) %>%
    mutate(focal.kin.available = sum (binary == "kin")) %>%
    mutate(order.of.partner = ifelse (DSI==0,0,rank(-DSI,ties.method= "min"))) %>%
    mutate(top3 = order.of.partner %in% 1:3) %>%
    mutate(per.kin.in.top3 = sum(binary == "kin" & top3 == T)/sum (top3 == T)) %>%
    mutate(per.nonkin.in.top3 = sum(binary == "non-kin" & top3 == T)/sum (top3 == T)) %>%
    mutate(groupyear = i) %>%
    mutate(group = str_extract(i,"[a-z]+")) %>%
    mutate(year = str_extract(i,"(\\d)+")) %>%
    ungroup()
  
  gy <-
    subset(
      gy,
      select = c(
        groupyear:year,
        focal.id,
        partner.id,
        r:relationship,
        DSI:per.nonkin.in.top3,
        focal.hrs.focalfollowed,
        partner.hrs.focalfollowed,
        groom.giving:proximity.rate.over.mean,
        focal.sex:focal.percofsex.dominanted,
        partner.sex:partner.percofsex.dominanted
      )
    )
  
  write.csv(gy,
            file = paste0(
              "~/Desktop/Non-kin cooperation data/wrangled data/",
              i,
              "DSI.csv"
            ))
  
  View(gy)
  
  return(sapply(gy, function(x)
    sum(is.na(x))))
}

lapply(groupyears, wrangling)

#kk2013 groomings are NA
#r2016 grooming a lot of NA and 0
#v2017 proximity scan number?
#r2016 proximity scan numbers very low (0-4)
