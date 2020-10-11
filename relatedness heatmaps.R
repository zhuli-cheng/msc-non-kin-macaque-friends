rm(list = ls())

library(RColorBrewer)

pedigree <-
  read.csv("~/Desktop/Non-kin cooperation data/pedigree.csv", header = T)

diffmom <-
  as.character(pedigree$DAM) %in% c("", as.character(pedigree$BEHAVIORAL.MOM))
print(pedigree[diffmom == F, ])

pedigree <- pedigree %>%
  select(ID, SEX, SIRE, BEHAVIORAL.MOM)

pedigree <- with(pedigree, kinship(ID, SIRE, BEHAVIORAL.MOM, SEX))

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

gyheatmap <- function (i) {
  gy<-read_csv (paste0("~/Desktop/Non-kin cooperation data/txt data/",i,".txt"),col_types = cols(sex = "f"))
  gy<-gy %>%
    subset(sex=="F")
  gy <- pedigree[as.character(gy$id), as.character(gy$id)]
  pdf(paste0("~/Desktop/Non-kin cooperation data/graphs/relatedness heatmaps/",i,"hm.pdf"))
  heatmap(gy,scale="column",Colv = NA, Rowv = NA, col = terrain.colors(256))
  dev.off()
}

lapply(groupyears, gyheatmap)

