source("LoadRpackages.R")

#define the 19 group-years
group.years <-
  c("F2010",
    "F2011",
    "F2012",
    "F2013",
    "F2014",
    "F2015",
    "F2016",
    "F2017",
    "HH2014",
    "HH2016",
    "KK2013",
    "KK2015",
    "KK2017",
    "R2015",
    "R2016",
    "S2011",
    "V2015",
    "V2016",
    "V2017")

#load 19 groupyears
ID.files <- do.call(rbind, lapply(group.years, function(gy) {
  path <- paste0("../input/NEW_raw_data/Group", gy, "_GroupByYear.txt")
    data <- read.table(path, header = TRUE, sep = ",")
    data$group.year <- gy
  return(data)  # Return the modified data frame
}))

ID.files$sex[ID.files$sex == "FALSE"] <- "F"

ID.files <- ID.files %>%
  separate(group.year, into = c("social.group", "year"), sep = "(?<=[a-zA-Z])(?=[0-9])", remove = FALSE)

#select females, of adult age, and having met focal observation cut-off
ID.files <- ID.files %>%
  filter(sex == "F") %>%
  filter(age >= 6) %>%
  filter(focalcutoff_met == 'Y')

#delete migrated females (focal, partner, mothers) from data
migration <- ID.files %>%
  dplyr::select (id, social.group) %>%
  unique() %>%
  group_by(id) %>%
  mutate(migration = n()) %>%
  subset(migration != 1) %>%
  ungroup()

unique(migration$id) #1 female migrated between groups

ID.files <- ID.files %>%
  subset(!id %in% migration$id) %>%
  subset(!mother %in% migration$id) 

#number of unique subject IDs
length(unique(ID.files$id))

#number of subject-years
length(ID.files$id)

#number of subjects per group-year
ID.files <- ID.files %>%
  group_by(group.year) %>%
  mutate(group.size = length(unique(id))) %>%
  ungroup()

group.sizes <- ID.files %>%
  select(group.year, group.size) %>%
  unique()
mean(group.sizes$group.size) #mean group size is 51.32
range(group.sizes$group.size) #range is 19-72

#availability of parentage information
#availability of SIRE information
confirm.sire <- read_tsv("../input/PEDIGREE.txt", show_col_types = FALSE) %>%
  setNames(c(
    "focal.id",
    "SEX",
    "BIRTH",
    "DOD",
    "REMOVE.DATE",
    "DAM",
    "SIRE",
    "BEHAVIORAL.MOM",
    "CS.STATUS",
    "GROUP.AT.BIRTH",
    "CS.BIRTH.SEASON",
    "Comments")) %>%
  dplyr::select(focal.id, SIRE, GROUP.AT.BIRTH)

confirm.sire <- confirm.sire %>%
  subset (focal.id %in% ID.files$id)

#number of females with SIRE information vs. without 
nrow(confirm.sire)
confirm.sire[is.na(confirm.sire$SIRE), ]
sum(is.na(confirm.sire$SIRE))#n=10

#availability and validity of behavioural mother data
confirm.mother <- read_tsv("../input/PEDIGREE.txt", show_col_types = FALSE)  %>%
  setNames(c(
    "focal.id",
    "SEX",
    "BIRTH",
    "DOD",
    "REMOVE.DATE",
    "DAM",
    "SIRE",
    "BEHAVIORAL.MOM",
    "CS.STATUS",
    "GROUP.AT.BIRTH",
    "CS.BIRTH.SEASON",
    "Comments")) %>%
  dplyr::select(focal.id, DAM, BEHAVIORAL.MOM, GROUP.AT.BIRTH) %>%
  mutate(BEHAVIORAL.MOM = ifelse(BEHAVIORAL.MOM == "?", NA, BEHAVIORAL.MOM))

confirm.mother <- confirm.mother %>%
  subset (focal.id %in% ID.files$id)

#number of all females vs. with behavioural mother corresponding/not corresponding to biological mother vs. na
nrow(confirm.mother)
sum(confirm.mother$DAM == confirm.mother$BEHAVIORAL.MOM, na.rm = T)#338

confirm.mother[confirm.mother$DAM != confirm.mother$BEHAVIORAL.MOM, ]
sum(confirm.mother$DAM != confirm.mother$BEHAVIORAL.MOM, na.rm = T)#2

confirm.mother[is.na(confirm.mother$DAM), ]
sum(is.na(confirm.mother$DAM))#7




