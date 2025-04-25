source("ChooseSubjects.R")

################### read in pedigree file ####################
#read pedigree dataset; set genetic mother as the mother. If genetic mother information is missing, use behavioural mother
pedigree.data <- 
  read_tsv("../input/PEDIGREE.txt") %>%
  setNames(c("ID","SEX","BIRTH","DOD","REMOVE.DATE","DAM","SIRE","BEHAVIORAL.MOM","CS.STATUS","GROUP.AT.BIRTH","CS.BIRTH.SEASON","Comments")) %>%
  mutate(BEHAVIORAL.MOM = ifelse(BEHAVIORAL.MOM != "?", BEHAVIORAL.MOM, NA)) %>% 
  mutate(mother = ifelse (is.na(DAM), BEHAVIORAL.MOM, DAM))

pedigree.data <- pedigree.data %>%
  filter(!is.na(ID)) 

#monkeys with behavioural mother information missing
pedigree.data[is.na(pedigree.data$BEHAVIORAL.MOM), ]
#only 3 males

#availability of genetic mother
nrow(pedigree.data[!is.na(pedigree.data$DAM), ])
nrow(pedigree.data[is.na(pedigree.data$DAM), ])

#availability of genetic father
nrow(pedigree.data[!is.na(pedigree.data$SIRE), ])
nrow(pedigree.data[is.na(pedigree.data$SIRE), ])

#monkeys whose genetic mother and behavioural mother do not match
diff.mom <- pedigree.data %>%
  filter(!is.na(BEHAVIORAL.MOM) & !is.na(DAM))
sum(diff.mom$BEHAVIORAL.MOM != diff.mom$DAM)
nrow(diff.mom)
sum(diff.mom$BEHAVIORAL.MOM == diff.mom$DAM)/nrow(diff.mom)

#age range of monkeys in pedigree data
range(as.Date(pedigree.data$BIRTH,"%d/%m/%Y"), na.rm = TRUE, finite = FALSE)
#"1985-01-01" "2016-09-29"

###################### build pedigree and calculate pairwise relatedness ##############################
#unique_ids <- unique(pedigree.data$ID)
#missing_dads <- dplyr::setdiff(unique(pedigree.data$SIRE), unique_ids)
#missing_moms <- dplyr::setdiff(unique(pedigree.data$mother), unique_ids)
#missing_ids <- unique(c(missing_dads, missing_moms))
#additional_data <- data.frame(ID = missing_ids)
#pedigree.data <- bind_rows(pedigree.data, additional_data)
#pedigree <- with(pedigree.data, kinship(ID, SIRE, mother, SEX))

pedigree.data <- with(pedigree.data, fixParents(ID, SIRE, mother, SEX))
pedigree.file <- with(pedigree.data, pedigree(id, dadid, momid, sex, missid = 0))
kinship <- kinship(pedigree.file)


###################### calculate pedigree depth ####################################
pedigree.depth <- with(pedigree.data, kindepth(id, dadid, momid, align = F))
pedigree.depth <- cbind(pedigree.data$id, pedigree.depth)

colnames(pedigree.depth) <- c("id", "depth")
pedigree.depth <- as.data.frame(pedigree.depth)


