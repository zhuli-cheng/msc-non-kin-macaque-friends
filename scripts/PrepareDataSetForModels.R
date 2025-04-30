source("ChooseSubjects.R")
#source("CalculateDSI.R"); run only if DSI files need to be updated


########################## read in DSI.files ########################################
all.dyads <- list()
for (gy in group.years) {
  all.dyads[[gy]] <- read.csv(paste0("../output/DSI/", gy, "DSI.csv"), colClasses = c(social.group = "character"))
}
all.dyads <- bind_rows(all.dyads)

all.dyads <- all.dyads %>%
  group_by(group.year) %>%
  mutate(group.size = length(unique(id))) %>%
  ungroup()

           
######################### descriptive statistics on the dataset #####################
range(all.dyads$r)#range of relatedness: 0-0.5625

#number of years a dyad was under observation
all.dyads <- all.dyads %>%
  mutate(dyad = paste(id, partner, sep = "+")) %>%
  group_by(dyad) %>%
  mutate(dyad.total.years = n()) %>%
  ungroup()


#descriptions on focal females
all.focal <- all.dyads %>%
  dplyr::select(
    id,
    social.group,
    year,
    age,
    focal.kin.available,
    percofsex.dominated,
    hrs.focalfollowed,
    focal.total.scan,
    group.size) %>%
  unique() %>%
  group_by(id) %>%
  mutate(focal.years = n()) %>%
  ungroup()


#distribution of age
range(all.focal$age)#6-28

#number of focal hours followed
plot(density(all.focal$hrs.focalfollowed))
mean(all.focal$hrs.focalfollowed)#5.33
range(all.focal$hrs.focalfollowed)#0.75 - 10.83

#number of scans on focal females
hist(all.focal$focal.total.scan, breaks = 153)
mean(all.focal$focal.total.scan)#95.75
range(all.focal$focal.total.scan)#18 - 195


#number of kin vs. nonkin groupmates
kin.nonkin <- all.dyads %>%
  dplyr::select(id, partner, kinship, year) %>%
  group_by (id, year) %>%
  unique() %>%
  mutate(no.kin = sum(kinship == "kin")) %>%
  mutate(no.nonkin = sum(kinship == "non-kin")) %>%
  dplyr::select(id, year, no.kin, no.nonkin) %>%
  unique() %>%
  mutate(nonkin.minus.kin = no.nonkin - no.kin) %>%
  mutate(kin.over.nonkin = no.kin / no.nonkin) %>%
  mutate(perc.kin = no.kin / (no.kin + no.nonkin)) %>%
  mutate(perc.nonkin = no.nonkin / (no.kin + no.nonkin)) 

mean(kin.nonkin$no.kin)#5.61
range(kin.nonkin$no.kin)#0 - 18

mean(kin.nonkin$no.nonkin)#50.23
range(kin.nonkin$no.nonkin)#14 - 71

mean(kin.nonkin$nonkin.minus.kin) #44.62
range(kin.nonkin$nonkin.minus.kin) #8-71, always more non-kin than kin

#check number of bonds
Top3.focal <- all.dyads %>%
  mutate(order.of.partner = as.factor(order.of.partner)) %>%
  dplyr::select(id, year, top3.total) %>%
  unique()

sum(Top3.focal$top3.total == 0)#5
sum(Top3.focal$top3.total == 1)#18
sum(Top3.focal$top3.total == 2)#38
sum(Top3.focal$top3.total == 3)#879
sum(Top3.focal$top3.total == 4)#31
sum(Top3.focal$top3.total == 5)#3
sum(Top3.focal$top3.total > 5)#1

#half the dataset because partner1&partner2 = partner2&partner1
half.dyads<- all.dyads %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(id, partner)), collapse = '+')) %>%
  group_by(social.group, year) %>%
  distinct(dyad)

half.dyads<- left_join(half.dyads, all.dyads, by = c("social.group", "year", "dyad")) %>%
  mutate(partner1 = id) %>%
  mutate(partner2 = partner) %>%
  dplyr::select(
    social.group,
    year,
    dyad,
    partner1,
    partner2,
    r,
    kinship,
    DSI,
    grooming_over_group,
    proximity_over_group,
    dyad.total.years)


cor(half.dyads$proximity_over_group, half.dyads$grooming_over_group, method = "pearson")


