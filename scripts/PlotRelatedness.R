source("CalculateRelatedness.R")

############ relatedness heatmap #############
#choose colors 
transparent_color <- rgb(1, 1, 1, alpha = 0)
#diag(data) <- transparent_color
blue_to_pink <- colorRampPalette(c("#00BFC4", "#66D8DB", "#FDD3C9", "#FDC9B7", "#FDB8B0", "#FDAF9F", "#FCACA0", "#FA9A8C", "#E55B4E"))
# 0.5625/9*2=0.125

#create a function for heatmaps
relatedness_heatmap <- function(gy) {
  # Prepare the dataset
  id <- ID.files$id[ID.files$group.year == gy]
  data <- kinship[id, id]
  data <- data*2

  # Plot the heatmap
  heatmap(data, scale = "none", Colv = NA, Rowv = NA, col = blue_to_pink(256),
 #         main = paste("Relatedness coefficients (r) between adult females in", gy),
          cexRow = 0.3, cexCol = 0.3,
          width = 6, height = 5, zlim = c(0, 0.5625))
  mtext(paste0("Figure S", i, ": Relatedness coefficients (r) between adult female subjects in ", gy, "."), side = 1, line = 3, adj = 0, cex = 0.8)
  
  # Create a color scale legend
  image.plot(z = matrix(0:1, nrow = 10), zlim = c(0, 0.5625), col = blue_to_pink(256), legend.only = TRUE, legend.width = 0.5,
             legend.shrink = 0.9, legend.mar = 5)
}


#save as one pdf
pdf("../output/supplementary_materials/relatedness heatmaps/Supplementary_Materials_Relatedness_coefficients_heatmaps.pdf", width = 8, height = 6)
i=0
for (gy in group.years) {
  i=i+1
  relatedness_heatmap(gy)
}
dev.off()

#save as separate svg
for (gy in group.years) {
  svg(filename = paste0("../output/supplementary_materials/relatedness heatmaps/separately/",gy,".svg"), width = 8, height = 7)
  relatedness_heatmap(gy)
  dev.off()
}

