source("PrepareDataSetForModels.R")

######################################################################
####### question 3: proportion of non-kin bond partners #############
######################################################################

#prepare dataframe for the model: numbers.all
numbers.all <- all.dyads %>%
  dplyr::select (
    id,
    social.group,
    year,
    top3.kin,
    top3.nonkin,
    top3.total,
    focal.connections,
    focal.kin.available,
    age,
    percofsex.dominated,
    group.size
  ) %>%
  unique() %>%
  subset(top3.total != 0) %>% #delete focal females with no bonds
  group_by(id) %>%
  mutate(n = n()) %>%
  ungroup()


#no. of unique subject 
length(unique(numbers.all$id))#345

#no. of female-years
length(numbers.all$id)#970, because 5 out of the 975 female-years had no bonds

#year of observation for each subject
years <- numbers.all %>%
  dplyr::select(id, n) %>%
  unique()

mean(years$n)#2.81 years
range(years$n)# 1 - 8 years

#availability of kin groupmates
mean(numbers.all$focal.kin.available)#5.62
range(numbers.all$focal.kin.available)# 0 - 18
sum(numbers.all$focal.kin.available==0)
sum(numbers.all$focal.kin.available==1)
sum(numbers.all$focal.kin.available==2)
sum(numbers.all$focal.kin.available>2)

#availability of kin and non-kin groupmates
mean(numbers.all$group.size)
range(numbers.all$group.size)

#distribution of female ages
mean(numbers.all$age)
range(numbers.all$age)

#model 3
model3 <- glmer (cbind(top3.nonkin, top3.kin) ~ 
                    focal.kin.available +
                    age +
                    percofsex.dominated +
                    group.size +
                    (1 | social.group) + 
                    (1 | id) + 
                    (1 | year), 
                  family = binomial,
                  data = numbers.all)


#assumptions 3
simout  <-  simulateResiduals (model3, n = 1000); plot(simout)
testOutliers(simout, type = 'bootstrap', nBoot = 100, plot = T)

vif(model3)
hist(intercep.nb <- coef(model3)$id[, 1], breaks = 100)
hist(intercep.nb <- coef(model3)$social.group[, 1], breaks = 100)
hist(intercep.nb <- coef(model3)$year[, 1], breaks = 100)
hist(model3@u)

qqPlot(residuals(model3))
plot(residuals(model3) ~ fitted(model3))

table(numbers.all$social.group)

#results 3
summary(model3)
drop1(model3, test = "Chisq")

#model output
fixed <- fixef(model3); fixed
confintfixed <- confint(model3, parm = "beta_", method = "Wald"); confintfixed

IRR <- exp(fixed); IRR #odds ratio
1/IRR - 1 #the odds of a social bond being non-kin increased by 19%

#plot
#figure 3a
kin.intervals <- seq(0, 18, by = 0.001)
predictions.3.kin.availability <- ggpredict(
  model = model3,
  terms = "focal.kin.available [kin.intervals]",
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
#  bias_correction = TRUE
)

predictions.3.kin.availability$predicted[predictions.3.kin.availability$x==5]
predictions.3.kin.availability$predicted[predictions.3.kin.availability$x==10]


figure3.kin.availability <- ggplot(data = numbers.all, aes(x = focal.kin.available, y = top3.nonkin/top3.total)) +
  geom_count(alpha = 0.6) +
  scale_size_area(breaks = c(1, 10, 50), limits = c(1, 100)) +
  geom_line(data = predictions.3.kin.availability, aes(x = x, y = predicted), color = "grey20", size = 1) +  # Predicted line
#  geom_line(data = predictions.3.average, aes(x = x, y = predicted), color = "red", size = 1) +  # Predicted line
  geom_ribbon(data = predictions.3.kin.availability, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), fill = "grey30", alpha = 0.2) +
  theme_AnimalBehaviour() +
  xlab("Number of available kin") +
  ylab ("Probability of bond partner being non-kin") +
  scale_x_continuous(breaks = seq(0, 18, by = 2)) +
  guides(size="none") +
  scale_clean_x() +
  scale_clean_y(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.25)); figure3.kin.availability


#ggsave("../output/figures_main_text/Figure3a.svg", plot = figure3.kin.availability, width = 10, height = 6, dpi = 1200, device = "svg")
#ggsave("../output/figures_main_text/Figure3.jpeg", plot = figure3, width = 10, height = 6, dpi = 1200, device = "jpeg")

#figure 3b
age.intervals <- seq(6, 28, by = 0.01)

predictions.3.age <- ggpredict(
  model = model3,
  terms = "age [age.intervals]",
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
  #  bias_correction = TRUE
)


figure3.age <- ggplot(data = numbers.all, aes(x = age, y = top3.nonkin/top3.total)) +
  geom_count(alpha = 0.6) +
  scale_size_area(breaks = c(1, 10, 50), limits = c(1, 100)) +
  geom_line(data = predictions.3.age, aes(x = x, y = predicted), color = "grey20", size = 1) +  # Predicted line
  geom_ribbon(data = predictions.3.age, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), fill = "grey30", alpha = 0.2) +
  theme_AnimalBehaviour() +
  xlab("Age (year)") +
  ylab(NULL) +
  guides(size="none") +
  scale_clean_x(breaks = seq(6, 28, by = 5)) +
  scale_clean_y(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.25)); figure3.age

#ggsave("../output/figures_main_text/Figure3b.svg", plot = figure3.age, width = 10, height = 6, dpi = 1200, device = "svg")

#figure 3c
rank.intervals <- seq(0, 100, by = 0.1)

predictions.3.rank <- ggpredict(
  model = model3,
  terms = "percofsex.dominated [rank.intervals]",
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
  #  bias_correction = TRUE
)


figure3.rank <- ggplot(data = numbers.all, aes(x = percofsex.dominated, y = top3.nonkin/top3.total)) +
  geom_count(alpha = 0.6) +
  scale_size_area(breaks = c(1, 10, 50), limits = c(1, 100)) +
  theme_AnimalBehaviour() +
  xlab("Rank (%)") +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  guides(size=guide_legend("No. of female-years")) +
  theme(legend.position = c(0.3, 0.06),
        legend.justification = c(0, 0), 
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white", colour = NA),
        legend.box.background = element_rect(colour = "black", size = 0.6),
        legend.key.size  = unit(2, "mm"),
        legend.key.width = unit(2, "mm"),
        legend.spacing.y = unit(0.5, "mm"),
        legend.box.margin = margin(1, 1, 1, 1)) +
  scale_clean_x() +
  scale_clean_y(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.25)) +
  annotate("text", x = 80, y = 0.9, label = "NS", size = 6, fontface = "italic"); figure3.rank


#ggsave("../output/figures_main_text/Figure3c.svg", plot = figure3.rank, width = 10, height = 6, dpi = 1200, device = "svg")

#combine the three plots in figure3
figure3 <- (figure3.kin.availability + figure3.age + figure3.rank) &
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(face = "plain"), plot.tag.position = c(0.1, 0.95), plot.tag.location = "panel"); figure3

#ggsave("../output/figures_main_text/Figure3.svg", plot = figure3, width = 12, height = 6, dpi = 1200, device = "svg")
ggsave("../output/figures_main_text/Figure3.jpeg", plot = figure3, width = 12, height = 6, dpi = 1200, device = "jpeg")




