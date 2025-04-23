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


######################### scramble IDs for publishing ###########################
unique.focal <- unique(all.dyads$id)
unique.partner <- unique(all.dyads$partner)
all.unique <- c(unique.focal, unique.partner)
all.dyads.unique <- data.frame(all.unique)
all.dyads.unique %<>%
  filter(!duplicated(all.unique))
all.dyads.unique.shuffled <- all.dyads.unique[sample(1:nrow(all.dyads.unique)), ]
all.dyads.unique.shuffled <- data.frame(all.dyads.unique.shuffled) %>%
  rename(all.unique = all.dyadsall.dyads %>%
  mutate(id = id_map[id],
         partner = id_map[partner]).unique.shuffled)
all.dyads.unique.shuffled %<>%
  mutate(new.id = row_number())

id_map <- setNames(all.dyads.unique.shuffled$new.id, all.dyads.unique.shuffled$all.unique)

all.dyads.scrambled.id <- all.dyads %>%
  mutate(id = id_map[id],
         partner = id_map[partner])

write.csv(all.dyads.scrambled.id, file = "../output/DSI_scrambled.csv")
