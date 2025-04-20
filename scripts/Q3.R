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
simout  <-  simulateResiduals (model3, n = 250); plot(simout)
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
CI <- exp(cbind(fixed, confintfixed)); CI

IRR <- exp(fixed); IRR #odds ratio
1/IRR - 1 #the odds of a social bond being non-kin increased by 19%

#plot
kin.intervals <- seq(0, 18, by = 0.001)
predictions.3 <- ggpredict(
  model = model3,
  terms = "focal.kin.available [kin.intervals]",
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
#  bias_correction = TRUE
)


figure3 <- ggplot(data = numbers.all, aes(x = focal.kin.available, y = top3.nonkin/top3.total)) +
  geom_count(alpha = 0.6) +
  scale_size_area(breaks = c(1, 10, 50)) +
  geom_line(data = predictions.3, aes(x = x, y = predicted), color = "grey20", size = 1) +  # Predicted line
#  geom_line(data = predictions.3.average, aes(x = x, y = predicted), color = "red", size = 1) +  # Predicted line
  geom_ribbon(data = predictions.3, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), fill = "grey30", alpha = 0.2) +
  theme_classic() +
  xlab("Number of available kin") +
  ylab ("Probability of bond partner being non-kin") +
  labs(title = "") +
  scale_x_continuous(breaks = seq(0, 18, by = 2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=18)) +
  guides(size=guide_legend("No. of subject-years")); figure3


ggsave("../output/figures_main_text/Figure3.svg", plot = figure3, width = 10, height = 6, dpi = 1200, device = "svg")
ggsave("../output/figures_main_text/Figure3.jpeg", plot = figure3, width = 10, height = 6, dpi = 1200, device = "jpeg")







#count the subjects that had at least 1 non-kin bonds when having ? available kin
newdata <- numbers.all %>%
  filter(focal.kin.available == 1)
nrow(newdata)

newdata.nonkin <- newdata %>%
  filter(top3.nonkin > 0) 
nrow(newdata.nonkin)/nrow(newdata)

newdata.only.nonkin <- newdata %>%
  filter(top3.kin == 0) 
nrow(newdata.only.nonkin)/nrow(newdata)

#calculate the estimated proportion of non-kin bonds 
newdata <- data.frame(
  focal.kin.available = 10,
  age = mean(numbers.all$age, na.rm = TRUE),  # Set other predictors as desired
  percofsex.dominated = mean(numbers.all$percofsex.dominated, na.rm = TRUE),
  group.size = mean(numbers.all$group.size, na.rm = TRUE),
  social.group = NA,  # Random effects left unspecified for marginal prediction
  id = NA,
  year = NA
)

predict(model3, newdata, type = "response", re.form = NA)

