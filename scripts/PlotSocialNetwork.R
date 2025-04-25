source("LoadRpackages.R")
source("ChooseSubjects.R")
#source("CalculateDSI.R"); only run when DSI needs to be updated

sna <- function (gy) {
  data <- read.csv(file = paste0("../output/DSI/", gy, "DSI.csv"), header = TRUE) %>%
    subset(DSI > 1) %>%
    mutate(DSI = ifelse(kinship == "kin", DSI, -DSI)) 
  dd <- data %>%
    dplyr::select(c("id", "partner", "DSI"))
  dd <- dcast(dd, id ~ partner, value.var = "DSI", fill = 0, drop = FALSE)
  dd <- as.matrix(dd)
  dd <- dd[ ,-1]

  set.seed(42)
  
  G <- graph.adjacency(dd, mode = "undirected", weighted = T) 
  E(G)$color <- ifelse(E(G)$weight < 0, "#00BFC4", "#E55B4E")
  E(G)$weight <- abs(E(G)$weight)
  V(G)$kin.available = (as.numeric(data$focal.kin.available[match(V(G)$name, data$id)]))
  V(G)$label.cex <- 0.5
  
  plot.igraph(G, vertex.size = (V(G)$kin.available) ^ (1/1.2),
              edge.width = E(G)$weight * 0.15,
              vertex.frame.color = "#ffffff",
              vertex.color = "grey")

  plot_info <- par("usr")
  x_bottom_center <- (plot_info[1] + plot_info[2])/2
  mtext(paste0("Figure S", i, ": Social network of ", gy,". The subjects had ", min(data$focal.kin.available), "-", max(data$focal.kin.available), " adult female kin groupmates."), side = 1, line = 3, adj = 0, cex = 0.8)
  }


#save as one pdf
i=19
pdf(paste0("../output/supplementary_materials/social networks/Supplementary_Materials_Social_networks.pdf"), width = 8, height = 6)
for (gy in group.years) {
  i=i+1
  sna(gy)
}
dev.off()

#save as separate svg
i=19
for (gy in group.years) {
  i=i+1
  svg(filename = paste0("../output/supplementary_materials/social networks/separately/", gy, ".svg"))
  sna(gy)
  dev.off()
}


############# F2010 for Figure 1c ##############
sna.figure1d <- function (gy) {
  data <- read.csv(file = paste0("../output/DSI/", gy, "DSI.csv"), header = TRUE) %>%
    subset(DSI > 1) %>%
    mutate(DSI = ifelse(kinship == "kin", DSI, -DSI)) 
  dd <- data %>%
    dplyr::select(c("id", "partner", "DSI"))
  dd <- dcast(dd, id ~ partner, value.var = "DSI", fill = 0, drop = FALSE)
  dd <- as.matrix(dd)
  dd <- dd[ ,-1]
  
  set.seed(42)
  
  G <- graph.adjacency(dd, mode = "undirected", weighted = T) 
  E(G)$color <- ifelse(E(G)$weight < 0, "#00BFC4", "#E55B4E")
  E(G)$weight <- abs(E(G)$weight)
  V(G)$kin.available = (as.numeric(data$focal.kin.available[match(V(G)$name, data$id)]))
  V(G)$label.cex <- 0.5
  
  plot.igraph(G, vertex.size = (V(G)$kin.available) ^ (1/1.2),
              edge.width = E(G)$weight * 0.3,
              vertex.frame.color = "#ffffff",
              vertex.color = "grey",
              vertex.label = NA,
              margin = c(-0.1))
  
}

svg(filename = "../output/figures_main_text/Figure1d.svg")
par(mar = c(0, 0, 0, 0))  # No margins (bottom, left, top, right)
sna.figure1d("F2010")
dev.off()

