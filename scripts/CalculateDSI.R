source("CalculateRelatedness.R")

#focal information
DSI <- function(gy) {
  IDs <- ID.files %>%
    filter(group.year == gy) %>%
    select(id, group.year, social.group, year, age, percofsex.dominated, hrs.focalfollowed) 
  
  IDs.partner <- IDs %>%
    mutate(partner = id) %>%
    mutate(hrs.partnerfollowed = hrs.focalfollowed) %>%
    select(partner, hrs.partnerfollowed)
  
  #relatedness
  IDs.r <- as.data.frame(as.table(kinship[as.character(IDs$id), as.character(IDs$id)])) %>%
    dplyr::rename(id = Var1, partner = Var2, r = Freq) %>%
    filter(id != partner) %>%
    mutate(r = r*2) %>%
    mutate(kinship = ifelse(r < 0.125, "non-kin","kin"))
  
  IDs.r <- full_join(IDs, IDs.r, by = "id")
  IDs.r <- full_join(IDs.r, IDs.partner, by = "partner")
  IDs.r <- IDs.r %>%
    select(id:partner, hrs.partnerfollowed, r, kinship)
  
  #grooming
  grooming.file <- read.table(paste0("../input/NEW_raw_data/Group", gy, "_GroomingEvents.txt"), header = TRUE, sep = ",")
  
  grooming.file <- grooming.file %>%
    filter(groom_giver %in% IDs$id, groom_reciever %in% IDs$id) %>%
    group_by(groom_giver, groom_reciever) %>%
    mutate(grooming_duration = sum(constrained_duration)) %>%
    ungroup() %>%
    select(groom_giver, groom_reciever, grooming_duration) %>%
    distinct()
  
  grooming.file <- grooming.file %>%
    rowwise() %>%
    mutate(pair = list(sort(c(groom_giver, groom_reciever)))) %>%
    ungroup() %>%
    mutate(id = sapply(pair, `[[`, 1),
           partner = sapply(pair, `[[`, 2)) %>%
    group_by(id, partner) %>%
    summarize(grooming_duration = sum(grooming_duration), .groups = "drop")
  
  grooming.file.reverse <- grooming.file %>%
    dplyr::rename(id = partner, partner = id) %>%
    select(id, partner, grooming_duration)
  
  grooming.file <- bind_rows(grooming.file, grooming.file.reverse)
  
  IDs.r.DSI <- left_join(IDs.r, grooming.file, by = join_by(id, partner)) %>%
    mutate(grooming_duration = replace_na(grooming_duration, 0)) %>%
    mutate(grooming_over_observation = grooming_duration/(hrs.focalfollowed + hrs.partnerfollowed)/3600) %>%
    mutate(grooming_over_group = grooming_over_observation/mean(grooming_over_observation))
  
  #proximity
  proximity.file <- read.table(paste0("../input/NEW_raw_data/Group", gy, "_ProximityGroups.txt"), header = TRUE, sep = ",")
  
  proximity.file <- proximity.file %>%
    dplyr::rename(id = focal.monkey) %>%
    select(id, in.proximity) %>%
    group_by(id) %>%
    mutate(focal.total.scan = n()) %>%
    ungroup()%>%
    separate_rows(in.proximity, sep = ",") %>%
    mutate(in.proximity = trimws(in.proximity)) %>%
    filter(id %in% IDs$id) %>%
    filter(in.proximity %in% IDs$id) %>%
    group_by(id, in.proximity) %>%
    mutate(focal.scans.with.partner = n()) %>%
    distinct() %>%
    ungroup() 

  
  proximity.partner <- proximity.file %>%
    dplyr::rename(partner.total.scan = focal.total.scan, partner.scans.with.focal = focal.scans.with.partner) %>%
    dplyr::rename(in.proximity = id, id = in.proximity)
  
  proximity.file <- full_join(proximity.file, proximity.partner, by = c("id", "in.proximity")) %>%
    dplyr::rename(partner = in.proximity) %>%
    group_by(partner) %>%
    mutate(partner.total.scan = toString(unique(na.omit(partner.total.scan)))) %>%
    ungroup() %>%
    group_by(id) %>%
    mutate(focal.total.scan = toString(unique(na.omit(focal.total.scan)))) %>%
    ungroup() 
  
  proximity.file[is.na(proximity.file)] <- 0
  
  proximity.file <- proximity.file %>%
    mutate(total_scans = as.numeric(focal.total.scan) + as.numeric(partner.total.scan)) %>%
    mutate(proximity_scans = focal.scans.with.partner + partner.scans.with.focal) %>%
    mutate(proximity_over_observation = proximity_scans/total_scans) %>%
    select(id, partner, focal.total.scan, proximity_scans, proximity_over_observation) 
  
  IDs.r.DSI <- full_join(IDs.r.DSI, proximity.file, by = c("id", "partner")) %>%
    mutate(proximity_over_observation = replace_na(proximity_over_observation, 0)) %>%
    group_by(id) %>%
    mutate(focal.total.scan = toString(unique(na.omit(focal.total.scan)))) %>%
    ungroup() %>%
    filter(id != partner) %>%
    mutate(proximity_over_group = proximity_over_observation/mean(proximity_over_observation)) %>%
    mutate(DSI = (grooming_over_group + proximity_over_group)/2)
  
  IDs.r.DSI <- IDs.r.DSI %>%
    group_by(id) %>%
    mutate(focal.connections = sum (DSI != 0)) %>%
    mutate(focal.kin.available = sum (kinship == "kin")) %>%
    mutate(order.of.partner = ifelse (DSI == 0, NA, rank(-DSI, ties.method= "min"))) %>%
    mutate(top3 = order.of.partner %in% 1:3 & DSI > 1) %>%
    mutate(top3.kin = sum(kinship == "kin" & top3 == T)) %>%
    mutate(top3.nonkin = sum(kinship == "non-kin" & top3 == T)) %>%
    mutate(top3.total = top3.kin + top3.nonkin) %>%
    ungroup()
  
  write.csv(IDs.r.DSI, paste0("../output/DSI/", gy, "DSI.csv"), row.names = FALSE)
}  

for (gy in group.years) {
  DSI(gy)
}

