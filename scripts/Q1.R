source("PrepareDataSetForModels.R")

#######################################
####### question 1a: DSI ~ r ##########
#######################################

#number of dyads
length(unique(half.dyads$dyad))#10994 unique dyads
nrow(half.dyads)#n = 27218 dyad-years

#number of dyads with DSI = 0
mean(half.dyads$DSI == 0)#78.72%
sum(half.dyads$DSI == 0)#21427

range(half.dyads$DSI)

#model 1a
DSI.r.tweedie <- glmmTMB(DSI  ~ r + 
                           (1 | partner1) + 
                           (1 | partner2) + 
                           (1 | dyad) + 
                           (1 | social.group) + 
                           (1 | year),
                         data = half.dyads,
                         REML = TRUE,
                         family=tweedie(link="log"))

#assumptions 1a
simout  <-  simulateResiduals (DSI.r.tweedie, n = 1000); plot(simout)
hist(intercep.tweedie <- ranef(DSI.r.tweedie)$cond$partner1[[1]], breaks = 100)
hist(intercep.tweedie <- ranef(DSI.r.tweedie)$cond$partner2[[1]], breaks = 100)
hist(intercep.tweedie <- ranef(DSI.r.tweedie)$cond$dyad[[1]], breaks = 100)
hist(intercep.tweedie <- ranef(DSI.r.tweedie)$cond$social.group[[1]], breaks = 100)
hist(intercep.tweedie <- ranef(DSI.r.tweedie)$cond$year[[1]], breaks = 100)

#assumptions: zero-inflation
100 * sum(half.dyads$DSI == 0) / nrow(half.dyads) #more than 70% is zero. test Zero inflation
testZeroInflation(simout) #data is not zero inflated

#results 1a
summary(DSI.r.tweedie)
family_params(DSI.r.tweedie)
VarCorr(DSI.r.tweedie)
#drop1(DSI.r.tweedie, test = "Chisq") #cannot use LRT when using REML

#model output
fixed <- fixef(DSI.r.tweedie); fixed
confintfixed <- confint(DSI.r.tweedie, parm = "beta_", method = "Wald"); confintfixed#Beware: The Wald method is less accurate but much faster.


#plot
r.intervals <- seq(0, 0.5625, by = 0.0025)
predictions.1a <- ggpredict(
  model = DSI.r.tweedie,
  terms = "r [r.intervals]",
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
  bias_correction = TRUE
)

#compare r=0 to r=0.5
predictions.1a$predicted[predictions.1a$x == 0]
predictions.1a$predicted[predictions.1a$x == 0.5]
predictions.1a$predicted[predictions.1a$x == 0.5]/predictions.1a$predicted[predictions.1a$x == 0]

figure1a <- ggplot(half.dyads, aes(x = r, y = DSI)) +
  geom_count(alpha = 3, aes(color = kinship)) +
  scale_size_area(breaks = c(1, 100, 1000, 13000)) +
  geom_line(data = predictions.1a, aes(x = x, y = predicted), color = "grey20", size = 1) +  # Predicted line
  geom_ribbon(data = predictions.1a, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), fill = "grey30", alpha = 0.2) +
  labs(x = "Relatedness (r)", y = "Relationship strength (DSI)", color = " ") +
  theme_classic() +
  guides(color = "none", size = guide_legend("No. of dyad-years")) + # Remove color legend, keep size legend
  scale_color_manual(values = c("#E55B4E", "#00BFC4"), labels = c("Kin", "Non-kin")) +
  scale_x_continuous(breaks = seq(0, 0.5, by = 0.125)) +
  scale_y_continuous(breaks = seq(0, 140, by = 20)) +
  theme(legend.position = c(0.05, 0.9),
        legend.justification = c(0, 1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)); figure1a


ggsave("../output/figures_main_text/Figure1a.svg", plot = figure1a, width = 10, height = 6, dpi = 1200, device = "svg")


#########################################
###### question 1b: top3 ~ kinship ######
#########################################

all.dyads.1b <- all.dyads %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(id, partner)), collapse = '+')) 

#sample size
nrow(all.dyads.1b) # n = 54436 dyad-years
all.dyads.1b$top3 <- as.numeric(all.dyads.1b$top3)


#model 1b
top3.kinship.REML <- glmmTMB(top3 ~ r + 
                               (1 | id) + 
                               (1 | partner) + 
                               (1 | dyad) +
                               (1 | social.group) + 
                               (1 | year),
                             family = binomial,
                             REML = TRUE,
                             data = all.dyads.1b)

#assumptions 1b
simout  <-  simulateResiduals (top3.kinship.REML, n = 1000); plot(simout)
hist(intercept <- ranef(top3.kinship.REML)$cond$id[, 1], breaks = 100)
hist(intercept <- ranef(top3.kinship.REML)$cond$partner[, 1], breaks = 100) #?
hist(intercept <- ranef(top3.kinship.REML)$cond$dyad[, 1], breaks = 100)
hist(intercept <- ranef(top3.kinship.REML)$cond$social.group[, 1], breaks = 100)
hist(intercept <- ranef(top3.kinship.REML)$cond$year[, 1], breaks = 100)

#results 1b
summary(top3.kinship.REML)
#drop1(top3.kinship.REML, test = "Chisq") #cannot use LRT when using REML

#model output: r
fixed <- fixef(top3.kinship.REML); fixed
confintfixed <- confint(top3.kinship.REML, parm = "beta_", method = "Wald"); confintfixed
#compare r=0 to r=0.5
exp(fixed$cond[1] + fixed$cond[2] * 0) #odds for r = 0 to form bonds
exp(fixed$cond[1] + fixed$cond[2] * 0)/(1 + exp(fixed$cond[1] + fixed$cond[2] * 0)) #probability
exp(fixed$cond[1] + fixed$cond[2] * 0.5) #odds for r = 0.5 to form bonds
exp(fixed$cond[1] + fixed$cond[2] * 0.5)/(1 + exp(fixed$cond[1] + fixed$cond[2] * 0.5)) #probability
exp(fixed$cond[1] + fixed$cond[2] * 0.5) / exp(fixed$cond[1] + fixed$cond[2] * 0) #odds ratio
exp(fixed$cond[1] + fixed$cond[2] * 0.5)/(1 + exp(fixed$cond[1] + fixed$cond[2] * 0.5))/(exp(fixed$cond[1] + fixed$cond[2] * 0)/(1 + exp(fixed$cond[1] + fixed$cond[2] * 0))) #probability "ratio"

#plot
predictions.1b <- ggpredict(
  model = top3.kinship.REML,
  terms = "r [r.intervals]",
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
)

predictions.1b$predicted[predictions.1b$x == 0] #probability
predictions.1b$predicted[predictions.1b$x == 0.5] #probability
predictions.1b$predicted[predictions.1b$x == 0.5]/predictions.1b$predicted[predictions.1b$x == 0]

tmp <- all.dyads %>%
  group_by(id, group.year) %>%
  mutate(m.5 = sum(r == 0.5)) %>%
  mutate(m.0 = sum(r==0)) %>%
  ungroup()

mean(tmp$m.5)
mean(tmp$m.0)

mean(tmp$m.5)*predictions.1b$predicted[predictions.1b$x == 0.5]
mean(tmp$m.0)*predictions.1b$predicted[predictions.1b$x == 0]


figure1b <- ggplot(all.dyads.1b, aes(x = r)) +
  geom_count(aes(y = top3, color = kinship), alpha = 3) +
  scale_size_area(breaks = c(1, 100, 10000, 32000)) +
  geom_line(data = predictions.1b, aes(x = x, y = predicted), color = "grey20", size = 1) +  # Predicted line
  geom_ribbon(data = predictions.1b, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), fill = "grey30", alpha = 0.2) +
  labs(x = "Relatedness (r)", y = "Probability of social bond") +
  theme_classic() +
  guides(color = "none", size = guide_legend("No. of dyad-years")) + # Remove color legend, keep size legend
  scale_color_manual(values = c("#E55B4E", "#00BFC4"), labels = c("Kin", "Non-kin")) +
  scale_x_continuous(breaks = seq(0, 0.5, by = 0.125)) +
  theme(legend.position = c(0.05, 0.9),
        legend.justification = c(0, 1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)); figure1b

ggsave("../output/figures_main_text/Figure1b.svg", plot = figure1b, width = 10, height = 6, dpi = 1200, device = "svg")


##################################
######### question 1c ###########
###################################
#figure 1c
Top3 <- all.dyads %>%
  subset(top3 == T) %>%
  mutate(order.of.partner = as.factor(order.of.partner))

give.n <- function(x) {
  return(c(y = median(x), label = length(x)))
}

figure1c <- ggplot(data = Top3, aes(x = order.of.partner, y = DSI + 0.001))  +
  geom_violin(aes(fill = kinship)) +
  facet_wrap(~ order.of.partner, scales = "free_x", labeller = labeller(order.of.partner = c("1" = "1st bond", "2" = "2nd bond", "3" = "3rd bond"))) +
  stat_summary(fun.data = give.n, geom = "text", fun = median, aes(group = kinship), position = position_dodge(width = 0.9), , hjust = 0.5, size = 3.5, color = "white") +
  scale_fill_manual(values = c("#E55B4E", "#00BFC4")) +
  theme_bw() +
  xlab(NULL) +
  ylab ("Relationship strength (DSI)") +
  scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 140)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 14, face = "bold")); figure1c

ggsave("../output/figures_main_text/Figure1c.svg", plot = figure1c, width = 10, height = 6, dpi = 1200, device = "svg")

#number of non-kin in a female's top 3
#filter female-years without social bonds
nonkin.in.top3 <- all.dyads %>%
  dplyr::select (id, social.group, year, top3.kin, top3.nonkin, top3.total) %>%
  unique() %>%
  subset(top3.total != 0)

nrow(nonkin.in.top3)#sample size = 970 female-years

#model 1c
model.nonkin <-glmer(cbind(top3.nonkin, top3.kin) ~ 
                        (1 | id) + 
                        (1 | social.group) + 
                        (1 | year), 
                      family = binomial,
                      data = nonkin.in.top3)
#both models gave the same results [using cbind (top3.nonkin, top3.total-top3.nonkin), and using top3.kin/top3.total]

#assumptions 1c
simout  <-  simulateResiduals (model.nonkin, n = 250); plot(simout)
hist(intercep <- coef(model.nonkin)$id[, 1], breaks = 100)
hist(intercep <- coef(model.nonkin)$social.group[, 1], breaks = 100)
hist(intercep <- coef(model.nonkin)$year[, 1], breaks = 100)
hist(model.nonkin@u,  breaks = 100)


#results 1c
summary(model.nonkin)
sum(nonkin.in.top3$top3.total)
#model result
fixed <- fixef(model.nonkin); fixed
confintfixed <- confint(model.nonkin, parm = "beta_", method = "Wald"); confintfixed

#interpretations
IRR <- exp(fixed); IRR #odds of the social bond being non-kin rather than kin
IRR / (IRR + 1) #expected proportion of non-kin among all bonds
1 / (IRR + 1) #expected proportion of kin among all bonds

########## Figure 1 ####################
figure1d <- readPicture("../output/figures_main_text/Figure1d.svg")
figure1d <- grImport2::pictureGrob(figure1d)

legend_plot <- ggplot(data.frame(x = c(1, 2), y = c(1, 2), group = c("Kin", "Non-kin")), aes(x, y, color = group)) +
  geom_point(size = 5) +
  scale_color_manual(values = c("Kin" = "#E55B4E", "Non-kin" = "#00BFC4")) +
  labs(color = "") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.background = element_blank(),  # Removes background color
        legend.key = element_blank()) +      # Removes key background
  guides(color = guide_legend(override.aes = list(size = 6))); legend_plot
legend <- get_legend(legend_plot)


figure1 <- ((figure1a + figure1b) / (figure1c + figure1d)) / legend +
  plot_annotation(tag_levels = list(c('1A', '1B', '1C', '1D'))) +
  plot_layout(heights = c(10, 10, 1), widths = c(1, 1)); figure1

ggsave("../output/figures_main_text/Figure1.svg", plot = figure1, width = 12, height = 9, dpi = 1200, device = "svg")
ggsave("../output/figures_main_text/Figure1.jpeg", plot = figure1, width = 12, height = 9, dpi = 1200, device = "jpeg")


