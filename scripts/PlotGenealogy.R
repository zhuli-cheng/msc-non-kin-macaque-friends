source("CalculateRelatedness.R")

############ genealogy ######################
genealogy <- function(gy) {
  ID <- ID.files$id[ID.files$group.year == gy]
  pedigree.data.gy <- pedigree.data %>%
    mutate(avail=ifelse(id %in% ID, 1, 0)) 
  
  pedigree.gy <- pedigree.shrink(pedigree.file, avail=pedigree.data.gy$avail)
  
  pdf(paste0("../output/supplementary_materials/genealogy/", gy, ".pdf"), width = 60, height = 8)
  par(mar = c(5, 4, 6, 2))
  plot.pedigree.shrink(pedigree.gy, align=F)
  dev.off()
}

for (gy in group.years) {
  genealogy(gy)
}
