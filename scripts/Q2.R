source("PrepareDataSetForModels.R")


####################################################
###### question 2a bond duration ##################
####################################################

#select dyads from group F and are top partners
f.top3.dyads <- unique(all.dyads$dyad[all.dyads$social.group == "F" & all.dyads$top3 == T])
#extract full data of these dyads
years.in.top3 <- all.dyads %>%
  subset (dyad %in% f.top3.dyads) %>%
  subset(dyad.total.years > 1)

years.in.top3 <- years.in.top3 %>%
  group_by(dyad) %>%
  mutate(years.in.top3 = sum(top3)) %>%
  mutate(maximal.dyadic.DSI = max(DSI)) %>%
  ungroup() %>%
  dplyr::select(
    r,
    kinship,
    id,
    partner,
    dyad,
    years.in.top3,
    dyad.total.years,
    maximal.dyadic.DSI) %>%
  unique()

#no. of females in this dataset
length(unique(years.in.top3$id))#102 focal females

#no. of dyad-years in this dataset: kin and non-kin dyads
nrow(years.in.top3)#959 dyad-years

#co-residing years
hist(years.in.top3$dyad.total.years)
mean(years.in.top3$dyad.total.years)#4.65 years
range(years.in.top3$dyad.total.years)# 2-8 years

years.in.top3 %>%
  group_by(kinship) %>%
  mutate(x = mean(dyad.total.years))

#bond years
hist(years.in.top3$years.in.top3)
mean(years.in.top3$years.in.top3)#1.55 years
range(years.in.top3$years.in.top3)# 1-7 years

#bond years if kin
mean(years.in.top3$years.in.top3[years.in.top3$kinship == "kin"])#2.63 years
range(years.in.top3$years.in.top3[years.in.top3$kinship == "kin"])# 1 - 7 years

#bond years if non-kin
mean(years.in.top3$years.in.top3[years.in.top3$kinship == "non-kin"])#1.22 years
range(years.in.top3$years.in.top3[years.in.top3$kinship == "non-kin"])# 1 - 5 years

nrow(years.in.top3[years.in.top3$kinship == "non-kin" & years.in.top3$years.in.top3 == 1,])/nrow(years.in.top3[years.in.top3$kinship == "non-kin",])
#83.08% of bond with non-kin only lasted one year


#model 2a
duration.kinship <- glmer(cbind(years.in.top3, (dyad.total.years - years.in.top3)) ~ maximal.dyadic.DSI + 
                            kinship + 
                            (1 | id) + 
                            (1 | partner),
                          data = years.in.top3, 
                          family = binomial)

#assumptions 2a
simout  <-  simulateResiduals (duration.kinship, n = 250); plot(simout)
vif(duration.kinship)
hist(intercep <- coef(duration.kinship)$id[, 1], breaks = 100)
hist(intercep <- coef(duration.kinship)$partner[, 1], breaks = 100)
hist(duration.kinship@u)
qqPlot(residuals(duration.kinship))
plot(residuals(duration.kinship) ~ fitted(duration.kinship))

#results 2a
summary(duration.kinship)
drop1(duration.kinship, test = "Chisq")

#model output
fixed <- fixef(duration.kinship); fixed
confintfixed <- confint(duration.kinship, parm = "beta_", method = "Wald"); confintfixed
IRR <- exp(cbind(fixed, confintfixed)); IRR #the odds ratio to remain a bond for non-kin versus kin
exp(fixed[1])/(1+exp(fixed[1])) #probability for kin to be bond partner is 
exp(fixed[1]+fixed[3])/(1+exp(fixed[1]+fixed[3])) #probability for non-kin to be bond partner

exp(fixed[1])
exp(fixed[1] + fixed[3])



#figure2a
figure2a <- ggplot(years.in.top3, aes(x= years.in.top3,  group=kinship)) +
  geom_bar(aes(y = ..prop.., stat="count")) +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 1),
                 y= ..prop.. ), stat= "count", vjust = -.1, size = 3) +
  labs(y = "Percentage of bonded dyads", x = "Total number of years bonded") +
  facet_grid(~kinship, labeller = labeller(kinship = c("kin" = "Kin", "non-kin" = "Non-kin"))) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks=c(1:8), labels=c(1:8),limits=c(0,8)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=12),
    axis.title=element_text(size=14),
    strip.text = element_text(size = 14, face = "bold")); figure2a

ggsave("../output/figures_main_text/Figure2a.svg", plot = figure2a, width = 10, height = 6, dpi = 1200, device = "svg")


####################################################
###### question 2b: bond variability #######
####################################################

#prepare data 
f.top3.dyads <- unique(all.dyads$dyad[all.dyads$social.group == "F" & all.dyads$top3 == T])

f.top3 <- all.dyads %>%
  subset(dyad %in% f.top3.dyads) %>%
  subset(dyad.total.years > 1)

variability <- f.top3 %>%
  group_by(dyad) %>%
  mutate(mean.dyadic.DSI = mean(DSI)) %>%
  mutate(sd.dyadic.DSI = sd(DSI)) %>%
  mutate(RSD = sd.dyadic.DSI / mean.dyadic.DSI) %>%
  ungroup() %>%
  dplyr::select(
    id,
    partner,
    dyad.total.years,
    mean.dyadic.DSI,
    sd.dyadic.DSI,
    RSD,
    r,
    kinship,
    dyad
  ) %>%
  unique()

variability$kinship <- factor(variability$kinship, ordered = FALSE)


nrow(variability) #959 dyads

#check NAs
sapply(variability, function(x) sum(is.na(x)))

#model 2b
variability.kinship <- lmer(RSD ~ kinship + 
                             (1 | id) + 
                             (1 | partner),
                           data = variability)


#assumptions 2b
simout  <-  simulateResiduals (variability.kinship, n = 250); plot(simout)
hist(intercep <- coef(variability.kinship)$id[, 1], breaks = 100)
hist(intercep <- coef(variability.kinship)$partner[, 1], breaks = 100)
hist(variability.kinship@u)
qqPlot(residuals(variability.kinship))
plot(residuals(variability.kinship) ~ fitted(variability.kinship))


#results 2b
summary(variability.kinship)
drop1(variability.kinship, test = "Chisq")

#model output
fixed <- fixef(variability.kinship); fixed
confintfixed <- confint(variability.kinship, parm = "beta_", method = "Wald"); confintfixed #Beware: The Wald method is less accurate but much faster.
fixed[1] #expected CV for kin = 0.93
fixed[1]+fixed[2] #expected CV for non-kin = 1.50

#relevel the kinship levels to get CI for non-kin
#variability$kinship <- relevel(variability$kinship, ref = "non-kin")

#plot variability 

predictions.2b <- ggpredict(
  model = variability.kinship,
  terms = "kinship",
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
  bias_correction = TRUE
)

figure2b <- ggplot(variability, aes(x = kinship, y = RSD)) +
  geom_violin() +  # Raw data (violin plot) +
  geom_jitter(width = 0.15, alpha = 0.5, color = "grey") +
  geom_point(data = predictions.2b, mapping = aes(x = x, y = predicted), size = 3) +
  geom_errorbar(data = predictions.2b, mapping = aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1.5) +
  scale_x_discrete(labels = c("Kin bond dyad", "Non-kin bond dyad")) +
  theme_classic() +
  labs(y = "Variability in relationship strength\n(CV in DSI)", x = "") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14)); figure2b

ggsave("../output/figures_main_text/Figure2b.svg", plot = figure2b, width = 10, height = 6, dpi = 1200, device = "svg")

#Figure 2
figure2 <- (figure2a + figure2b)  +
  plot_annotation(tag_levels = 'A', tag_prefix = '2') +
  plot_layout(widths = c(1, 1)); figure2

ggsave("../output/figures_main_text/Figure2.svg", plot = figure2, width = 10, height = 6, dpi = 1200, device = "svg")
#ggsave("../output/figures_main_text/Figure2.jpeg", plot = figure2, width = 10, height = 6, dpi = 1200, device = "jpeg")