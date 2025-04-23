source("PrepareDataSetForModels.R")
source("CalculateRelatedness.R")

##########################################################
########### Supplementary materials and tests ############
##########################################################

#################### Model summary #######################
#Use an excel table

##################### Relatedness Heatmap #################
#Use separate R script

##################### Social network plots ################
#Use separate R script

###################### pedigree depth #####################
pedigree.depth <- left_join(all.focal, pedigree.depth, by = "id")
pedigree.depth <- pedigree.depth %>%
  select(id, social.group, year, focal.kin.available, depth) %>%
  unique()

figure.sup1 <- ggplot(pedigree.depth, aes(x = focal.kin.available, y = depth)) +
  geom_count(alpha = 3) +
  scale_size_area(breaks = c(1, 10, 20, 30, 40)) +
  theme_classic() +
  theme(axis.text.x  = element_text(vjust = 0.5, size = 12, colour = "black")) +
  scale_x_continuous(breaks = seq(0, 18, by = 2)) +
  theme(axis.text.y  = element_text(vjust = 0.5, size = 12, colour = "black")) +
  theme(axis.title = element_text(size = 14, vjust = -5))  +
  labs(color = " ", x = "Number of available kin", y = "Pedigree depth", size = "No. of female-years"); figure.sup1
ggsave("../output/figures_main_text/FigureA2.svg", plot = figure.sup1, width = 6, height = 4, dpi = 1200, device = "svg")
#ggsave("../output/figures_main_text/PedigreeDepthKinAvailability.jpeg", plot = figure.sup1, width = 6, height = 4, dpi = 1200, device = "jpeg")


pedigree.depth.group <- pedigree.depth %>%
  select(id, social.group, depth) %>%
  unique()

pedigree.depth.group$depth <- as.numeric(pedigree.depth.group$depth)
mean(pedigree.depth.group$depth) #mean depth is 3.22
range(pedigree.depth.group$depth) # range: 1-6

figure.sup2 <- ggplot(pedigree.depth.group, aes(x = depth)) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~toupper(social.group))+
  labs(title = "",
       x = "Pedigree depth",
       y = "Number of subjects") +
  scale_x_continuous(breaks=c(0:6), labels=c(0:6),limits=c(0,6)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 14)); figure.sup2
ggsave("../output/figures_main_text/FigureA1.svg", plot = figure.sup2, width = 6, height = 4, dpi = 1200, device = "svg")
#ggsave("../output/figures_main_text/PedigreeDepthByGroup.jpeg", plot = figure.sup2, width = 6, height = 4, dpi = 1200, device = "jpeg")



############### DSI (~r, grooming and proximity) #############
#plot: DSI by kinship (kin vs nonkin)
figure.sup3 <- ggplot(half.dyads, aes(x = r, y = DSI + 0.001, color = kinship)) +
  scale_color_manual(values = c("#E55B4E", "#00BFC4"), labels = c("Kin dyad", "Non-kin dyad")) +
  geom_count(alpha = 3) +
  scale_y_continuous(trans = "log10", labels = c (0.001, 0.01, 0.1, 1, 10, 100), breaks = c(0.001, 0.01, 0.1, 1, 10, 100)) +
  scale_size_area(breaks = c(1, 100, 1000, 13000)) +
  labs (x = "Pairwise relatedness (r)", y = "DSI (+0.001)") + 
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour = "gray", size = 0.1)) +
  theme(axis.text.x  = element_text(vjust=0.5, size=12, colour="black")) +
  theme(axis.text.y  = element_text(vjust=0.5, size=12, colour="black")) +
  theme(axis.title = element_text(size=14, vjust = -5)) + 
  guides(size=guide_legend(c("No. of dyad-years"))) +
  labs(color = " ") ; figure.sup3

ggsave("../output/supplementary_materials/RelatednessDSI.svg", plot = figure.sup3, width = 6, height = 4, dpi = 1200, device = "svg")


p2 <- ggplot(half.dyads, aes(
  x = DSI + 0.001, color = kinship)) +
  geom_density() +
  scale_color_manual(values = c("#E55B4E", "#00BFC4"), labels = c("Kin dyad", "Non-kin dyad")) +
  scale_x_continuous(trans = "log10", labels = c (0.001, 0.01, 0.1, 1, 10, 100), breaks = c(0.001, 0.01, 0.1, 1, 10, 100)) +
  labs(title = NULL) + 
  theme_classic() + 
  xlab("DSI (+0.001)") +
  ylab ("Density") +
  theme(axis.text.x  = element_text(vjust=0.5, size=9, colour="black")) +
  theme(axis.text.y  = element_text(vjust=0.5, size=11, colour="black")) +
  theme(axis.title = element_text(size=12, vjust = -5)) + 
  theme(legend.title=element_blank()); p2


#plot the correlation between grooming and proximity, grouped by kinship
figure.sup4 <- ggplot(half.dyads, aes(
  x = grooming_over_group + 0.001,
  y = proximity_over_group + 0.001,
  color = kinship)) +
  scale_color_manual(values = c("#E55B4E", "#00BFC4"), labels = c("Kin", "Non-kin")) +
  scale_x_continuous(trans = "log10", labels = c (0.001, 0.01, 0.1, 1, 10, 100), breaks = c(0.001, 0.01, 0.1, 1, 10, 100)) +
  scale_y_continuous(trans = "log10", labels = c (0.001, 0.01, 0.1, 1, 10, 100), breaks = c(0.001, 0.01, 0.1, 1, 10, 100)) +
  geom_count(alpha = 3) +
  labs(title = "Correlation between dyadic grooming and proximity rates") +
  labs(title = NULL) +
  scale_size_area(breaks = c(1, 100, 1000, 19000)) +
  theme_classic() +
  xlab("Grooming rate (+0.001)") +
  ylab ("Proximity rate (+0.001)") +
  theme(axis.text.x  = element_text(vjust=0.5, size=12, colour="black")) +
  theme(axis.text.y  = element_text(vjust=0.5, size=12, colour="black")) +
  theme(axis.title = element_text(size=14, vjust = -5)) + 
  guides(size=guide_legend(c("No. of dyads"))) +
  labs(color = " ") +
  coord_fixed(ratio = 1); figure.sup4

ggsave("../output/supplementary_materials/GroomingProximity.svg", plot = figure.sup4, width = 6, height = 4, dpi = 1200, device = "svg")
ggsave("../output/supplementary_materials/GroomingProximity.jpeg", plot = figure.sup4, width = 6, height = 4, dpi = 1200, device = "jpeg")
