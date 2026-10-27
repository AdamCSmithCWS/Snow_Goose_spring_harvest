#### full Eastern Analysis for Spring Snow Goose Survey
library(tidyverse)
library(sf)
diry <- "Eastern" #paste("m:/My Documents/Snow Geese/A229/Quebec",Y,sep = "")


y = 2026

ly = y-1

prov_sel <- c("ON","PQ")




reg_sampled_on <- read.table(paste0(diry,"/SSGS ",y," ON.txt")) %>%
  rename(permit = 1) %>%
  mutate(id = (y-1)*1000000+permit)

reg_sampled_qc <- read.table(paste0(diry,"/SSGS ",y," PQ.txt")) %>%
  rename(permit = 1) %>%
  mutate(id = (y-1)*1000000+permit)

reg_sampled <- bind_rows(reg_sampled_on,reg_sampled_qc)



final_permits <- readxl::read_xlsx(paste0("data/protected/Permits 2025 (as of May 27 2026).xlsx")) %>%
  rename_with(., .fn = str_to_lower) %>%
  rename(permit_type = 'permit type') %>%
  mutate(id = (y-1)*1000000+permit)

if(length(which(is.na(final_permits$id))) > 0){
  stop("Some rows of permit file have no id - permit number")
}

spring <- final_permits %>%
  mutate(analysis_province = ifelse(!is.na(hunting_province),
                                    hunting_province, sampling_prov),
         analysis_province = ifelse(is.na(analysis_province),
                                    province, analysis_province), # prov is hunting prov, then sampling prov, then residence
         analysis_lat = ifelse(!is.na(hunting_latitude),
                               hunting_latitude, salelat),
         analysis_lat = ifelse(is.na(analysis_lat),
                               residence_latitude,analysis_lat),
         analysis_long = ifelse(!is.na(hunting_longitude),
                                hunting_longitude, salelon),
         analysis_long = ifelse(is.na(analysis_long),
                                residence_longitude,analysis_long)) %>%
  filter(grepl("pring",permit_type),
         analysis_province %in% prov_sel) %>%
  mutate(sampled = ifelse(grepl("not selected",permit_type,fixed = TRUE) |
                            grepl("outside",permit_type,fixed = TRUE),
                          FALSE,TRUE),
         season = "spring")


# for regular season, the residence is the most relevant location information
regular_season <- final_permits %>%
  mutate(analysis_province = ifelse(!is.na(province),
                                    province, sampling_prov),
         analysis_province = ifelse(is.na(analysis_province),
                                    hunting_province, analysis_province), # prov is residence prov, then sampling prov, then hunting
         analysis_lat = ifelse(!is.na(residence_latitude),
                               residence_latitude,salelat), # latitude is residence, then sale, then hunting
         analysis_lat = ifelse(is.na(analysis_lat),
                               hunting_latitude,analysis_lat),
         analysis_long = ifelse(!is.na(residence_longitude),# longitude is residence, then sale, then hunting
                                residence_longitude,salelon),
         analysis_long = ifelse(is.na(analysis_long),
                                hunting_longitude,analysis_long))%>%
  filter(grepl("egular",permit_type),
         analysis_province %in% prov_sel) %>%
  mutate(sampled = ifelse(id %in% reg_sampled$id,
                           TRUE,FALSE),
         season = "regular")

if(table(regular_season$sampled)["TRUE"] != nrow(reg_sampled)){
  stop("Some sampled permits are missing from the permit file")
}



full_permits <- bind_rows(spring,regular_season) %>%
  mutate(id = (ly*1000000) + permit) %>%
  # filter(!is.na(analysis_lat),
  #        !is.na(analysis_long)) %>%
  mutate(latd = floor(analysis_lat/100),
         latm = (analysis_lat/100 - latd)*100,
         latdd = latd + latm/60,
         longd = floor(analysis_long/100),
         longm = (analysis_long/100 - longd)*100,
         longdd = -1*(longd + longm/60))

tst <- full_permits %>%
  filter(is.na(latdd))

if(nrow(tst) != 0){
  stop("some permits are missing latitude coordinates")
}

full_permits <- full_permits %>%
  filter(!is.na(latdd))




# Spatial data for Eastern zones ------------------------------------------

# 1, it generates a spatial map of the eastern hunting strata
#based on the spring sampling zones in quebec and ontario

QC_zones <- readRDS("GIS/Quebec_historical_hunting_zones.rds") %>%
  st_transform(.,crs = 3348) #%>%  #Canada Lambert Conformal Conic (equidistance)


wmu.samp <- c("65","64A","64B","66A","67","68B","69A-1","69A-2","69A-3","69B","70","71")   ### vector of the Snow Goose wmus, in the correct order for the tables in the report


### zone P == WMU 65
### zone Q == outside WMU 65 in Ontario
###
wmu <- readRDS("GIS/Ontario_wmu_spring_Snow_goose_zones.rds") %>%
  rename(zone = hunt_zone) %>%
  select(zone) %>%
  group_by(zone) %>%
  summarise()
#plot(wmu)


all_eastern_map <- bind_rows(QC_zones,wmu) %>%
  ungroup() %>%
  mutate(zone = as.character(zone)) %>%
  group_by(zone) %>%
  summarise()
plot(all_eastern_map)



# Linking permits to geographic strata ------------------------------------


permit_map <- st_as_sf(full_permits,
                       coords = c("longdd","latdd")) %>%
  st_set_crs(4326) #4269 #NAD83 - assumption that this is probably what these coordinates represent

laea = st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area

 permit_map <- st_transform(permit_map, st_crs(all_eastern_map))
# all_eastern_map <- st_transform(all_eastern_map,laea)
#all_eastern_map <- st_buffer(all_eastern_map,dist = 10000)

 ### this step can take 15 minutes or more ...
permit_zone1 <- st_join(permit_map,all_eastern_map,
                        join = st_intersects,
                        largest = TRUE)# largest means only the fields of y with largest overlap - using largest = TRUE ensures that no rows are replicated if their points intersect more than one polygon



# Drop all permits outside of sampling regions ----------------------------
# except spring sampled permits

permit_zone <- permit_zone1 %>%
  filter((!is.na(zone) |
            grepl("Spring",permit_type) |
            permit %in% reg_sampled$permit)) #%>%
  # mutate(zone = ifelse(is.na(zone) &
  #                        analysis_province == "ON",
  #                      "P",zone),
  #        zone = ifelse(is.na(zone) &
  #                        analysis_province == "PQ",
  #                      "H",zone))

near <- permit_zone %>%
  filter(is.na(zone))

# nearest neighbour repair for any permits that fall outside of zone map (usually just a few at a border)
if(nrow(near) > 0){
  dist_sel_near <- near %>%
    select(-zone) %>%
    nngeo::st_nn(.,all_eastern_map)



  for(j in 1:nrow(near)){
    p <- near$permit[j]
    wz <- dist_sel_near[[j]]
    z <- all_eastern_map$zone[wz]

    wp <- which(permit_zone$permit == p)
    if(length(wp) > 1){stop("repeated permit values ",p)}
    permit_zone[wp,"zone"] <- z
  }

}


permit_zone <- permit_zone %>%
  mutate(zone = ifelse(sampled & analysis_province == "PQ"
                       & zone %in% c("A","B","C","Q") & season == "regular",
                       "H",zone),
         zone = ifelse(sampled & analysis_province == "ON"
                       & zone %in% c("Q") & season == "regular",
                       "P",zone))


regular_sampled_map <- permit_zone %>%
  filter(sampled,
         season == "regular",
         zone %in% c("A","B","C","Q"))

samp_bb <- st_bbox(regular_sampled_map)
tst <- ggplot()+
  geom_sf(data = all_eastern_map,
          aes(fill = zone),
          alpha = 0.2)+
  geom_sf(data = regular_sampled_map,
          aes(colour = zone))+
  coord_sf(xlim = samp_bb[c("xmin","xmax")],
           ylim = samp_bb[c("ymin","ymax")])

tst


full_sampled <- permit_zone %>%
  sf::st_drop_geometry() %>%
  filter(sampled)

pops_strat <- permit_zone %>%
  sf::st_drop_geometry() %>%
  group_by(zone,season) %>%
  summarise(population = n(),
            .groups = "keep")
pops_strat

sampled_pops_strat <- permit_zone %>%
  sf::st_drop_geometry() %>%
  filter(sampled) %>%
  group_by(zone,season) %>%
  summarise(sample = n(),
            .groups = "keep")

sampled_pops_strat
hunt_zones <- factor(c("D","E","F","G","H","I","P","O"),ordered = TRUE)



add1 <- permit_zone %>%
  sf::st_drop_geometry() %>%
  filter(zone %in% hunt_zones) %>%
  group_by(zone) %>%
  summarise(total_permit_pop_incl_spring = n(),
            .groups = "drop") %>%
  mutate(zones = factor(zone,levels = levels(hunt_zones))) %>%
  arrange(zones) %>%
  select(-zone)


add2 <- permit_zone %>%
  sf::st_drop_geometry() %>%
  filter(season == "spring") %>%
  group_by(zone) %>%
  summarise(total_spring_perms = n(),
            .groups = "drop") %>%
  mutate(proportion_of_spring_perms = round(total_spring_perms/sum(total_spring_perms),2),
         zones = factor(zone,levels = levels(hunt_zones))) %>%
  arrange(zones) %>%
  select(-zone)

# base of table 1, just needs the response rates
regular_season_sampling_summary <- permit_zone %>%
  sf::st_drop_geometry() %>%
  filter(season == "regular",
         zone %in% hunt_zones) %>%
  group_by(zone) %>%
  summarise(total_permit_pop = n(),
            sampled = length(which(sampled)),
            .groups = "drop") %>%
  mutate(proportion_of_pop = round(total_permit_pop/sum(total_permit_pop),2),
         zones = factor(zone,levels = levels(hunt_zones))) %>%
  arrange(zones) %>%
  inner_join(add1, by = "zones") %>%
  inner_join(add2, by = "zones")



table1_spring_rows <- permit_zone %>%
  sf::st_drop_geometry() %>%
  filter(season == "spring") %>%
  summarise(total_permit_pop = n(),
            sampled = length(which(sampled)),
            .groups = "drop") %>%
  mutate(analysis_stratum = "S")

table1_total_row <- permit_zone %>%
  sf::st_drop_geometry() %>%
  filter(zone %in% hunt_zones | season == "spring") %>%
  summarise(total_permit_pop = n(),
             total_permit_pop_incl_spring = length(which(zone %in% hunt_zones)),
            sampled = length(which(sampled)),
            .groups = "drop")%>%
  mutate(analysis_stratum = "T")

# Responses ---------------------------------------------------------------

q1 <- readxl::read_xls(paste0(diry,"/Ques1_3.xls"),
                       col_types = c("numeric",
                                     "text",
                                     rep("numeric",2))) %>%
  rename(id = ID,
         active = Q1,
         days = Q2,
         kill = Q3) %>%
  mutate(active = str_to_upper(active),
         kill = ifelse(id %in% c(2024363439,
                             2024066500),
                   NA,kill))





#################################################
if(y == 2025){
# One-off fill of missing N responses to Q1 ----------------------------------------------
#
# q1_raw <- readxl::read_xls(paste0(diry,"/Ques1_3.xls"),
#                            col_types = c("numeric",
#                                          "text",
#                                          rep("numeric",2))) %>%
#   rename(id = ID)

ids <- readxl::read_xls("Eastern/ID.xls",
                        col_types = c(rep("numeric",2))) %>%
  rename(id = ID) %>%
  select(-Index)


q1 <- q1 %>%
  full_join(ids,
            by = "id") %>%
  mutate(active = ifelse(is.na(active),
                         "N",active))




}


#################################################


# Calendar summary and plotting -------------------------------------------


bags <- readxl::read_xls(paste0(diry,"/Ques4.xls"),
                         col_types = c(rep("numeric",4))) %>%
  rename_with(.,.fn = str_to_lower) %>%
  mutate(date = ymd(paste(y,month,day,sep = "/")),
         killed = ifelse(killed < 0,NA,killed),
         killed = ifelse(killed > 50,50,killed)) %>%
  filter(!is.na(date),
         !is.na(killed)) %>%
  #select(id,killed,date) %>%
  arrange(date)

lw_not_na <- function(x){
  length(which(!is.na(x)))
}

cal_response_by_permit <- bags %>%
  select(-c(month,day)) %>%
  pivot_wider(id_cols = id,
              names_from = date,
              values_from = killed,
              names_prefix = "Day_") %>%
  rowwise() %>%
  mutate(cal_kil_sum = sum(c_across(starts_with("Day_")),na.rm = TRUE),
         cal_days_sum = lw_not_na(c_across(starts_with("Day_"))))#

resp_combined <- q1 %>%
  left_join(cal_response_by_permit,
            by = "id") %>%
  mutate(kill = ifelse(cal_kil_sum > kill,
                       cal_kil_sum,kill),
         days = ifelse(cal_days_sum > days,
                       cal_days_sum,days))


plot(resp_combined$kill,resp_combined$cal_kil_sum)
table(resp_combined$kill,resp_combined$cal_kil_sum, useNA = "always")
plot(resp_combined$days,resp_combined$cal_days_sum)






numerate <- function(x){
  seq(1,length((x)))
}

saint_fix <- function(x,prov){
  snt <- vector("character",length(x))
  x <- str_to_lower(x)
  for(i in 1:length(x)){
    if(prov[i] == "ON"){
      tmp <- x[i]
      snt[i] <- tmp
    }else{
      tmp <- str_replace(x[i],"^st ","saint ")
      #tmp <- str_replace(tmp,"^sainte ","saint ")
      tmp <- str_replace(tmp,"^ste ","sainte ")
      snt[i] <- tmp
    }
  }
  return(snt)
}
technique <- readxl::read_xls(paste0(diry,"/Ques5.xls"),
                              col_types = c("numeric",
                                            "text",
                                            rep("numeric",6)))%>%
  rename_with(.,.fn = str_to_lower) %>%
  filter(!is.na(id),
         !is.na(locality),
         locality != "PQ") %>%
  arrange(id) %>%
  mutate(loc_n = numerate(id),
         .by = id) %>%
  mutate(town = str_extract(locality,".+(?=,)"),
         prov = str_extract(locality,"(?<=,)[[:blank:]][[:graph:]]+"),
         prov = str_trim(ifelse(is.na(prov),
                       str_extract(locality,"[[:upper:]]{2}$"),
                       prov)),
         prov = ifelse(prov == "PQ",
                       "QC",prov),
         prov = ifelse(prov %in% c("ON","QC"),
                       prov,""),
         town = ifelse(is.na(town),
                       locality,town),
         town = saint_fix(town,prov)
         ) %>%
  filter(prov %in% c("QC","ON")) # drops reports of hunting in western hunt (1 or 2 every year)





# location information for fixing missing towns ---------------------------


previous_town_names <- read_csv(paste0(diry,"/townnamesprevious_years.csv"),
                                locale = locale(encoding = "WINDOWS-1252")) %>%
  distinct() %>%
  rename(locality = TOWN)


technique2 <- technique %>%
  select(locality) %>%
  distinct() %>%
  fuzzyjoin::stringdist_left_join(.,
                     y = previous_town_names,
                     by = c("locality"),
                     distance_col = "distance") %>%
  filter(!is.na(distance)) %>%
  group_by(locality.x) %>%
  filter(distance == min(distance,na.rm = T)) %>%
  rename(zone = DISTRICT,
         locality = locality.x) %>%
  select(-c(locality.y,distance)) %>%
  filter(!duplicated(locality)) %>%
  distinct()


technique <- technique %>%
  left_join(technique2,
            by = c("locality"))






re_do_canada_town_names <- FALSE

if(re_do_canada_town_names){
can_town_names <- sf::read_sf("gis/cgn_canada_shp_eng.shp")



towns_by_district <- can_town_names %>%
  mutate(town = str_to_lower(GEONAME)) %>%
  filter(PROV_TERR %in% c("Ontario","Quebec")) %>%
  select(PROV_TERR,town,REL_SCALE) %>%
  distinct() %>%
  mutate(prov = ifelse(PROV_TERR == "Ontario",
                       "ON","QC")) %>%
  st_transform(crs = st_crs(all_eastern_map))

## this one takes > 30 minutes
towns_by_district1 <- st_join(towns_by_district,all_eastern_map,
                                             join = st_intersects,
                                             largest = TRUE)# largest means only the fields of y with largest overlap - using largest = TRUE ensures that no rows are replicated if their points intersect more than one polygon


near <- towns_by_district1 %>%
  filter(is.na(zone))

# nearest neighbour repair for any permits that fall outside of zone map (usually just a few at a border)
if(nrow(near) > 0){
  dist_sel_near <- near %>%
    select(-zone) %>%
    nngeo::st_nn(.,all_eastern_map)



  for(j in 1:nrow(near)){
    p <- near$town[j]
    pr <- near$prov[j]
    wz <- dist_sel_near[[j]]
    z <- all_eastern_map$zone[wz]

    wp <- which(towns_by_district1$town == p & towns_by_district1$prov == pr)
    #if(length(wp) > 1){stop("repeated town names ",p)}
    towns_by_district1[wp,"zone"] <- z
  }

}




tst <- ggplot()+
  geom_sf(data = all_eastern_map)+
  geom_sf(data = towns_by_district1,
          aes(colour = zone))

tst


saveRDS(towns_by_district1,paste0(diry,"/Eastern_towns_mapped_to_districts.rds"))
}else{
  towns_by_district1 <- readRDS(paste0(diry,"/Eastern_towns_mapped_to_districts.rds"))
}

towns_by_district_list <- towns_by_district1 %>%
  st_drop_geometry()%>%
  filter(zone %in% c("F","H","O","I","G",
                     "D","E")) %>%
  mutate(town = gsub("-"," ",town)) %>%
  as.data.frame() %>%
  distinct()



# fuzzy matching of town names --------------------------------------------

technique_miss <- technique %>%
  filter(is.na(zone) | zone == "") %>%
  select(locality,town,prov) %>%
  distinct()

technique_not_miss <- technique %>%
  filter(!locality %in% technique_miss$locality) %>%
  select(locality,zone) %>%
  distinct()

dist_test <- fuzzyjoin::stringdist_left_join(x = technique_miss,
                                             y = towns_by_district_list,
                                             by = c("prov","town"),
                                             distance_col = "t_dist")



alt_match <- fuzzyjoin::stringdist_join(
  technique_miss, towns_by_district_list,
  by = c("prov","town"),  # Column to match on
  mode = "left",  # Type of join: left, inner, or full
  method = "jw",  # Jaro-Winkler distance metric
  max_dist = 0.18,  # Maximum allowable distance for a match
  distance_col = "distance"
)



best_match1 <- dist_test %>%
  mutate(REL_SCALE = as.integer(REL_SCALE)) %>%
  filter(prov.t_dist == 0) %>%
  group_by(locality) %>%
  filter(town.t_dist == min(town.t_dist) & REL_SCALE == max(REL_SCALE)) %>%
  filter(!duplicated(locality))

best_match2 <- alt_match %>%
  mutate(REL_SCALE = as.integer(REL_SCALE)) %>%
  filter(prov.distance == 0) %>%
  group_by(locality) %>%
  filter(town.distance == min(town.distance) & REL_SCALE == max(REL_SCALE))%>%
  filter(!duplicated(locality))


miss_matches <- best_match1 %>%
  filter(!locality %in% best_match2$locality) %>%
  select(locality,town.x,prov.x,zone) %>%
  rename(town = town.x,
         prov = prov.x)


best_matches <- best_match2 %>%
  select(locality,town.x,prov.x,zone) %>%
  rename(town = town.x,
         prov = prov.x) %>%
  bind_rows(miss_matches)


technique_miss2 <- technique_miss %>%
  filter(!locality %in% best_matches$locality) %>%
  mutate(zone = "")

# this re-edit value must be changed manually -----------------------------
# if set to TRUE, it allows for the editing and overwriting of temp_techn_manual_fill.rds
# this manual editing is useful if the fuzzy matching has not linked townnames to a zone
re_edit <- FALSE
  #this purposeful code failure is here to ensure that the manual editing is not
  #overwritten by accident
if(re_edit){
# Edit the remaining missing zones by hand --------------------------------
# can be done with google maps or by searching in the csv file for similar terms
technique_miss2 <- edit(technique_miss2)

saveRDS(technique_miss2,"temp_techn_manual_fill.rds")
}else{
  technique_miss2 <- readRDS("temp_techn_manual_fill.rds")
}

tech_w_zone <- technique %>%
  filter(!is.na(zone) | zone == "") %>%
  select(locality,zone) %>%
  distinct()

all_matches <- best_matches %>%
  bind_rows(technique_miss2) %>%
  select(locality,zone) %>%
  bind_rows(tech_w_zone) %>%
  distinct()

technique <- technique %>%
  select(-zone) %>%
  left_join(all_matches,
            by = "locality")



# end_zone ----------------------------------------------------------------



reported_tech <- technique %>%
  select(id) %>%
  distinct() %>%
  mutate(reported_tech = "Y")


tech_join <- technique %>%
  pivot_wider(values_from = c(days_pass : killed_blind_decoy,zone),
              id_cols = id,
              names_from = loc_n,
              names_prefix = "location_") %>%
  left_join(reported_tech)

if(any(is.na(tech_join$zone_location_1))){
  stop("At least one location_1 is missing zone assignment")
}

q6 <- readxl::read_xls(paste0(diry,"/Ques6_8.xls"),
                       col_types = c("numeric",
                                     rep("text",3)))%>%
  rename_with(.,.fn = str_to_lower)%>%
  mutate(used_recordings = str_to_upper(used_recordings),
         used_guide = str_to_upper(used_guide),
         hunted_in_fall = str_to_upper(hunted_in_fall))

comments <- readxl::read_xls(paste0(diry,"/Comments.xls"),
                             col_types = c("numeric",
                                           "text"))%>%
  rename_with(.,.fn = str_to_lower) %>%
  mutate(comments = str_to_lower(comments),
         flag = ifelse(grepl("guide",comments) | grepl("group",comments),
                       TRUE,FALSE))



resp_combined <- resp_combined %>%
  left_join(.,q6,
            by ="id") %>%
  left_join(.,comments,
            by = "id") %>%
  relocate(id,
           active,
           days,
           kill,
           used_recordings,
           used_guide,
           hunted_in_fall,
           comments,
           flag,
           cal_days_sum,
           cal_kil_sum)


# Full combined responses and reconciliation ------------------------------------

resp_combined2 <- resp_combined  %>%
  left_join(.,tech_join,
            by = "id") %>%
  group_by(id) %>%
  mutate(tech_kill_sum = sum(c_across(starts_with("killed_")),na.rm = TRUE), # sum of all reported kills by technique
         tech_days_max = max(c_across(starts_with("days_")),na.rm = TRUE), # sum of all reported days by technique
         tech_kill_sum = ifelse(is.na(tech_kill_sum),
                                0,tech_kill_sum), # zero fill tech kill
         tech_days_max = ifelse(is.na(tech_days_max),
                                0,tech_days_max), # zero fill tech days

         tech_kill_sum1 = sum(c_across(starts_with("killed_") & ends_with("_location_1")),na.rm = TRUE), # sum of all reported kills by technique location 1
         tech_days_max1 = max(c_across(starts_with("days_") & ends_with("_location_1")),na.rm = TRUE),# sum of all reported days by technique location 1

         tech_kill_sum2 = sum(c_across(starts_with("killed_") & ends_with("_location_2")),na.rm = TRUE),# sum of all reported kills by technique location 2
         tech_days_max2 = max(c_across(starts_with("days_") & ends_with("_location_2")),na.rm = TRUE),# sum of all reported days by technique location 2

         tech_kill_sum3 = sum(c_across(starts_with("killed_") & ends_with("_location_3")),na.rm = TRUE), # sum of all reported kills by technique location 3
         tech_days_max3 = max(c_across(starts_with("days_") & ends_with("_location_3")),na.rm = TRUE),# sum of all reported days by technique location 3

         tech_days_max1 = ifelse(is.finite(tech_days_max1),
                                 tech_days_max1,NA), # missing tech days fill with NA
         tech_days_max2 = ifelse(is.finite(tech_days_max2),
                                 tech_days_max1,NA), # missing tech days fill with NA
         tech_days_max3 = ifelse(is.finite(tech_days_max3),
                                 tech_days_max1,NA), # missing tech days fill with NA

         success_kill1 = ifelse(!is.na(tech_kill_sum1) & tech_kill_sum1 > 0, # Corrected success tech location 1
                                "Y","N"),
         success_kill2 = ifelse(!is.na(tech_kill_sum2) & tech_kill_sum2 > 0,# Corrected success tech location 2
                                "Y","N"),
         success_kill3 = ifelse(!is.na(tech_kill_sum3) & tech_kill_sum3 > 0,# Corrected success tech location 3
                                "Y","N"),

         active1 = ifelse(!is.na(tech_days_max1) & tech_days_max1 > 0,# Corrected active tech location 1
                          "Y","N"),
         active2 = ifelse(!is.na(tech_days_max2) & tech_days_max2 > 0,# Corrected active tech location 2
                          "Y","N"),
         active3 = ifelse(!is.na(tech_days_max3) & tech_days_max3 > 0,# Corrected active tech location 3
                          "Y","N"),

         cal_kil_sum = ifelse(is.na(cal_kil_sum), # zero filling cal kil total
                              0,cal_kil_sum),
         cal_days_sum = ifelse(is.na(cal_days_sum), # zero filling cal days total
                               0,cal_days_sum),
         days = ifelse(is.na(days), # zero filling days
                       0,days),
         days = ifelse(cal_days_sum > days,
                       cal_days_sum,days), # correcting days to
         days = ifelse((tech_days_max > days),
                       tech_days_max,days),
         kill = ifelse(is.na(kill),
                       0,kill),
         kill = ifelse(cal_kil_sum > kill,
                       cal_kil_sum,kill),
         kill = ifelse(tech_kill_sum > kill,
                       tech_kill_sum,kill),
         success_kill = ifelse(kill > 0,
                               "Y","N"),
         active = ifelse(kill > 0 | days > 0,
                         "Y",active),
         active = ifelse(days == 0 & !is.na(days),"N",active),
         active = ifelse(is.na(active),"N",active),
         days = ifelse((active == "Y") & is.na(days),
                       1,days),
         used_guide = ifelse(is.na(used_guide) | active == "N",
                             "N",
                             used_guide),
         used_recordings = ifelse(is.na(used_recordings) | active == "N",
                                  "N",used_recordings),
         hunted_in_fall = ifelse(is.na(hunted_in_fall) | active == "N",
                                 "N",hunted_in_fall),
         days_blind = ifelse(is.finite(sum(c_across(starts_with("days_blind_")),na.rm = TRUE)),
                             sum(c_across(starts_with("days_blind_")),na.rm = TRUE),
                             0),
         days_pass = ifelse(is.finite(sum(c_across(starts_with("days_pass_")),na.rm = TRUE)),
                            sum(c_across(starts_with("days_pass_")),na.rm = TRUE),
                            0),
         days_stalking = ifelse(is.finite(sum(c_across(starts_with("days_stalking_")),na.rm = TRUE)),
                                sum(c_across(starts_with("days_stalking_")),na.rm = TRUE),
                                0),
         killed_blind = ifelse(is.finite(sum(c_across(starts_with("killed_blind_")),na.rm = TRUE)),
                               sum(c_across(starts_with("killed_blind_")),na.rm = TRUE),
                               0),
         killed_pass = ifelse(is.finite(sum(c_across(starts_with("killed_pass_")),na.rm = TRUE)),
                              sum(c_across(starts_with("killed_pass_")),na.rm = TRUE),
                              0),
         killed_stalking = ifelse(is.finite(sum(c_across(starts_with("killed_stalking_")),na.rm = TRUE)),
                                  sum(c_across(starts_with("killed_stalking_")),na.rm = TRUE),
                                  0))%>%
  mutate(used_blind = ifelse((days_blind + killed_blind) > 0,
                             "Y",
                             "N"),
         used_pass = ifelse((days_pass + killed_pass) > 0,
                            "Y",
                            "N"),
         used_stalking = ifelse((days_stalking + killed_stalking) > 0,
                                "Y",
                                "N")) %>%
  relocate(id,
           active,
           days,
           kill,
           used_recordings,
           used_guide,
           hunted_in_fall,
           comments,
           flag,
           cal_days_sum,
           cal_kil_sum,
           reported_tech,
           starts_with("zone"),
           starts_with("days_"),
           starts_with("killed_"))

### note: warnings about "no non-missing arguments to max; returning -Inf"
### can be ignored

### note: warnings about "no non-missing arguments to max; returning -Inf"
### can be ignored



# Manual review of comments to ID group harvests --------------------------

possible_groups <- resp_combined2 %>%
  filter(flag, active == "Y")

possible_groups

## enter manual adjustments here
## e.g.,
# resp_combined2[which(resp_combined2$id == 2025063700),"days"] <- 1

# Manual review of harvest to ID group harvests ---------------------------

flag_group_harvest <- 150 # possible reporting for a group of hunters or a guide

possible_group_harv <- resp_combined2 %>%
  filter(kill > flag_group_harvest | kill/days > 50)
possible_group_harv %>% select(id,days,kill,comments)

probable_group_harv <- possible_group_harv %>%
  filter(kill/days > 50)
probable_group_harv #review these records for possible data-entry errors and group reporting
# if comments indicate number in group, then divide evenly among number of hunters
# if no comments ask Michel to confirm with paper response
# if not a data entry error and mean daily bag > 50, then drop permit from sample
## enter manual adjustments here



# filtering responses to sampled ------------------------------------------


strata <- full_sampled %>%
  select(id,zone,season)

out_range <- resp_combined2 %>%
  filter(!id %in% full_sampled$id)
table(out_range$zone_location_1)
## check to see if any of the removed responses are for the region of interest
## if they are, check to make sure they are not sampled in alternate region (e.g., in North)
out_check <- out_range %>%
  select(id) %>%
  unlist()
### these unsampled permits may indicate a hunter that hunted outside of where they
### were selected, or who responded using the wrong permit number
### this should be very few responses
out_check_perms <- resp_combined2 %>%
  filter(id %in% out_check)



# Final harvest file with permits ids strata etc --------------------------


harv <- resp_combined2 %>%
  filter(!(id %in% out_check),
         id %in% full_sampled$id) %>%
  left_join(strata,by = "id") %>%
  mutate(analysis_stratum = ifelse(season == "spring",
                                   "S",zone))


drop_no_tech <- which(harv$active == "Y" & is.na(harv$reported_tech))

paste(length(drop_no_tech)," active responses did not indicate techniques or locations")

check_no_tech <- harv[drop_no_tech,]

# Table 1 Sampling summary ------------------------------------------------

analysis_strata <- factor(c(levels(hunt_zones),"S","T"),
                          ordered = TRUE)

strata_responses <- harv %>%
  ungroup() %>%
  group_by(analysis_stratum) %>%
  summarise(returned = n()) %>%
  mutate(analysis_stratum = factor(analysis_stratum,levels = levels(analysis_strata)))

strata_responses_tot <- harv %>%
  ungroup() %>%
  summarise(returned = n()) %>%
  mutate(analysis_stratum = "T")

strata_responses <- strata_responses %>%
  bind_rows(strata_responses_tot)

table_1 <- regular_season_sampling_summary %>%
  mutate(analysis_stratum = zone) %>%
  bind_rows(table1_spring_rows) %>%
  bind_rows(table1_total_row) %>%
  left_join(strata_responses,by = "analysis_stratum") %>%
  mutate(prop_response = round(returned/sampled,2)) %>%
  select(-c(zone,zones)) %>%
  ungroup() %>%
  relocate(analysis_stratum,
           total_permit_pop,
           proportion_of_pop,
           total_permit_pop_incl_spring,
           sampled,
           returned,
           prop_response,
           total_spring_perms,
           proportion_of_spring_perms)


strata_names <- tibble(analysis_stratum = sort(analysis_strata),
                           `Province` = c(rep("Quebec",6),
                                             rep("Ontario",2),
                                             rep("",2)),
                           Stratum = c(sort(levels(analysis_strata))[1:6],
                                       "WMU 65","Other WMUs",
                                       "Spring Permits",
                                       "Total"))

table_1_out <- strata_names %>%
  inner_join(table_1, by = "analysis_stratum") %>%
  mutate(`Total Number of Permits in Sample (proportional distribution among hunting districts)` = paste0(total_permit_pop," (",proportion_of_pop,")"),
         `Total Number of Permits in Hunting District` = total_permit_pop_incl_spring,
         `Sample` = sampled,
         `Returned (%)` = paste0(returned," (",prop_response,")"),
         `Number of Spring Permits in Hunting District (proportional distribution)` = paste0(total_spring_perms," (",proportion_of_spring_perms,")"),
         across(contains("proportional distribution"),.fns = ~gsub("(NA)","",.x,fixed = TRUE))) %>%
  select(`Province`,
         Stratum,
         `Total Number of Permits in Sample (proportional distribution among hunting districts)`,
         `Total Number of Permits in Hunting District`,
         `Sample`,
         `Returned (%)`,
         `Number of Spring Permits in Hunting District (proportional distribution)`)


saveRDS(table_1_out, paste0(diry,"/tables/Table_1.rds"))


pops <- table_1 %>%
  filter(analysis_stratum != "T") %>%
  select(analysis_stratum,total_permit_pop) %>%
  rename(population = total_permit_pop)

harv <- harv %>%
  left_join(pops,
            by = "analysis_stratum")
saveRDS(harv,"Eastern/harv.rds")




# Table 2 -----------------------------------------------------------------


# hunting activity by sampling stratum

# Active, successful, days, harvest



prop_func <- function(x,success = "Y",
                      pop = 1000,
                      val = "mean"){
  if(length(unlist(pop)) > 1){
    pop <- unique(pop)
  }
  x <- unlist(x)

  y <- ifelse(!is.na(x),
              x,0)
  y <- ifelse(y == success,1,0)
  m_y = mean(y)*pop
  se_y = sqrt((var(y)*pop^2)/length(y))
  if(val == "se" & !is.na(se_y) & se_y > 0){
    return(round(se_y))
  }else{
    return(round(m_y))
  }

}


numeric_func <- function(x,
                         pop = 1000,
                         val = "mean"){
  if(length(unlist(pop)) > 1){
    pop <- unique(pop)
  }
  x <- as.numeric(unlist(x))
  y <- ifelse(!is.na(x),
              x,0)
  m_y = mean(y)*pop
  se_y = sqrt((var(y)*pop^2)/length(y))

  if(val == "se" & !is.na(se_y) & se_y > 0){
    return(round(se_y))
  }else{
    return(round(m_y))
  }

}


# Overall Harvest Estimates -----------------------------------------------

vals <- c("active_hunters",
          "success_hunters",
          "days_hunting",
          "harvest")
hdrs <- c("Number of Active Hunters",
          "Number of Successful Hunters",
          "Number of Days",
          "Harvest")


vals <- data.frame(val = vals,
                   type = c("prop",
                            "prop",
                            "num",
                            "num"),
                   hdr = hdrs)



active_hunters <- harv %>%
  ungroup() %>%
  group_by(analysis_stratum) %>%
  summarise(active_hunters_pop = prop_func(active,
                                           success = "Y",
                                           pop = population,
                                           val = "mean"),
            active_hunters_se_pop = prop_func(active,
                                              success = "Y",
                                              pop = population,
                                              val = "se"))


harv <- harv %>%
  left_join(active_hunters,
            by = "analysis_stratum")


#     table2 <- harv %>%
#       ungroup() %>%
#       filter(active == "Y") %>%
#       group_by(analysis_stratum) %>%
#       summarise(active_hunters = unique(active_hunters_pop),
#                 active_hunters_se = unique(active_hunters_se_pop),
#             success_hunters = prop_func(success_kill,
#                                        success = "Y",
#                                        pop = active_hunters_pop,
#                                        val = "mean"),
#             success_hunters_se = prop_func(success_kill,
#                                           success = "Y",
#                                           pop = active_hunters_pop,
#                                           val = "se"),
#             days_hunting = numeric_func(days,
#                                         pop = active_hunters_pop,
#                                         val = "mean"),
#             days_hunting_se = numeric_func(days,
#                                            pop = active_hunters_pop,
#                                            val = "se"),
#             harvest = numeric_func(kill,
#                                    pop = active_hunters_pop,
#                                    val = "mean"),
#             harvest_se = numeric_func(kill,
#                                       pop = active_hunters_pop,
#                                       val = "se"),
#             mean_kill = mean(kill,na.rm = TRUE),
#             max_kill = max(kill,na.rm = TRUE),
#             median_kill = median(kill))
#
#
#







table2 <- harv %>%
  ungroup() %>%
  group_by(analysis_stratum) %>%
  summarise(active_hunters = prop_func(active,
                                       success = "Y",
                                       pop = population,
                                       val = "mean"),
            active_hunters_se = prop_func(active,
                                          success = "Y",
                                          pop = population,
                                          val = "se"),
            success_hunters = prop_func(success_kill,
                                        success = "Y",
                                        pop = population,
                                        val = "mean"),
            success_hunters_se = prop_func(success_kill,
                                           success = "Y",
                                           pop = population,
                                           val = "se"),
            days_hunting = numeric_func(days,
                                        pop = population,
                                        val = "mean"),
            days_hunting_se = numeric_func(days,
                                           pop = population,
                                           val = "se"),
            harvest = numeric_func(kill,
                                   pop = population,
                                   val = "mean"),
            harvest_se = numeric_func(kill,
                                      pop = population,
                                      val = "se"),
            mean_kill = mean(kill,na.rm = TRUE),
            max_kill = max(kill,na.rm = TRUE),
            median_kill = median(kill))










table2_tot <- table2 %>%
  ungroup() %>%
  summarise(active_hunters = sum(active_hunters),
            active_hunters_se = round(sqrt(sum(active_hunters_se^2))),
            success_hunters = sum(success_hunters),
            success_hunters_se = round(sqrt(sum(success_hunters_se^2))),
            days_hunting = sum(days_hunting),
            days_hunting_se = round(sqrt(sum(days_hunting_se^2))),
            harvest = sum(harvest),
            harvest_se = round(sqrt(sum(harvest_se^2)))) %>%
  mutate(analysis_stratum = "T")

table_2 <- table2 %>%
  bind_rows(table2_tot)



table_2_out <- strata_names %>%
  inner_join(table_2, by = "analysis_stratum") %>%
  mutate(`Estimated Number of Active Hunters` = paste0(active_hunters," ± ",active_hunters_se),
         `Estimated Number Successful` = paste0(success_hunters," ± ",success_hunters_se),
         `Estimated Number of Days` = paste0(days_hunting," ± ",days_hunting_se),
         `Estimated Harvest` = paste0(harvest," ± ",harvest_se)) %>%
  select(`Province`,
         Stratum,
         `Estimated Number of Active Hunters`,
         `Estimated Number Successful`,
         `Estimated Number of Days`,
         `Estimated Harvest`)

saveRDS(table_2_out, paste0(diry,"/tables/Table_2.rds"))



# Table 3 -----------------------------------------------------------------

# reporting hunting location
# by zone of sample
# summary for spring permits


# n respondents active
# n reported location
# % of active
# n reported hunting outside district of sampling
# % of those reporting
# n hunt in > 1 district
# % of those reporting
#

table_3 <- harv %>%
  mutate(zone = ifelse(zone %in% c("A","B","C","Q"),
                       "outside",zone)) %>%
  # filter(reported_tech == "Y") %>%
  group_by(zone) %>%
  summarise(n_active = length(which(active == "Y")),
            n_report_loc = length(which(active == "Y" &
                                          reported_tech == "Y" &
                                          !is.na(zone_location_1))),
            n_outside_district = length(which((!is.na(zone_location_1) & zone_location_1 != zone) |
                                                (!is.na(zone_location_2) & zone_location_2 != zone) |
                                                (!is.na(zone_location_3) & zone_location_3 != zone))),
            n_GT_1_district = length(which((!is.na(zone_location_2) &
                                             zone_location_1 != zone_location_2) |
                                             (!is.na(zone_location_3) &
                                                zone_location_1 != zone_location_2 |
                                                zone_location_2 != zone_location_3)))) %>%
  mutate(p_report_loc = round(100*n_report_loc/n_active),
        p_outside_district = round(100*n_outside_district/n_report_loc),
        p_hunt_GT_1_district = round(100*n_GT_1_district/n_report_loc)) %>%
  relocate(zone,
           n_active,
           n_report_loc,
           p_report_loc,
           n_outside_district,
           p_outside_district,
           n_GT_1_district,
           p_hunt_GT_1_district)




table_3_sp <- harv %>%
  mutate(zone = ifelse(zone %in% c("A","B","C","Q"),
                       "outside",zone)) %>%
  ungroup() %>%
  # filter(reported_tech == "Y") %>%
  filter(season == "spring") %>%
  summarise(n_active = length(which(active == "Y")),
            n_report_loc = length(which(active == "Y" &
                                          reported_tech == "Y" &
                                          !is.na(zone_location_1))),
            n_outside_district = length(which((!is.na(zone_location_1) & zone_location_1 != zone) |
                                                (!is.na(zone_location_2) & zone_location_2 != zone) |
                                                (!is.na(zone_location_3) & zone_location_3 != zone))),
            n_GT_1_district = length(which((!is.na(zone_location_2) &
                                              zone_location_1 != zone_location_2) |
                                             (!is.na(zone_location_3) &
                                                zone_location_1 != zone_location_2 |
                                                zone_location_2 != zone_location_3)))) %>%
  mutate(p_report_loc = round(100*n_report_loc/n_active),
         p_outside_district = round(100*n_outside_district/n_report_loc),
         p_hunt_GT_1_district = round(100*n_GT_1_district/n_report_loc)) %>%
  relocate(n_active,
           n_report_loc,
           p_report_loc,
           n_outside_district,
           p_outside_district,
           n_GT_1_district,
           p_hunt_GT_1_district) %>%
  mutate(zone = "S")


table_3_tot <- harv %>%
  mutate(zone = ifelse(zone %in% c("A","B","C","Q"),
                       "outside",zone)) %>%
  # filter(reported_tech == "Y") %>%
  ungroup() %>%
  summarise(n_active = length(which(active == "Y")),
            n_report_loc = length(which(active == "Y" &
                                          reported_tech == "Y" &
                                          !is.na(zone_location_1))),
            n_outside_district = length(which((!is.na(zone_location_1) & zone_location_1 != zone) |
                                                (!is.na(zone_location_2) & zone_location_2 != zone) |
                                                (!is.na(zone_location_3) & zone_location_3 != zone))),
            n_GT_1_district = length(which((!is.na(zone_location_2) &
                                              zone_location_1 != zone_location_2) |
                                             (!is.na(zone_location_3) &
                                                zone_location_1 != zone_location_2 |
                                                zone_location_2 != zone_location_3)))) %>%
  mutate(p_report_loc = round(100*n_report_loc/n_active),
         p_outside_district = round(100*n_outside_district/n_report_loc),
         p_hunt_GT_1_district = round(100*n_GT_1_district/n_report_loc)) %>%
  relocate(n_active,
           n_report_loc,
           p_report_loc,
           n_outside_district,
           p_outside_district,
           n_GT_1_district,
           p_hunt_GT_1_district)%>%
  mutate(zone = "T")


table_3 <- table_3 %>%
  bind_rows(table_3_tot) %>%
  bind_rows(table_3_sp)%>%
  rename(analysis_stratum = zone)



table_3_out <- strata_names %>%
  inner_join(table_3, by = "analysis_stratum") %>%
  mutate(`Number of Respondents Active` = n_active,
         `Reported Location (% of Active)` = paste0(n_report_loc," (",p_report_loc,")"),
         `Hunt Outside District of Purchase (% of Reporting)` = paste0(n_outside_district," (",p_outside_district,")"),
         `Hunt in More Than One District (% of Reporting)` = paste0(n_GT_1_district," (",p_hunt_GT_1_district,")")) %>%
  select(`Province`,
         Stratum,
         `Number of Respondents Active`,
         `Reported Location (% of Active)`,
         `Hunt Outside District of Purchase (% of Reporting)`,
         `Hunt in More Than One District (% of Reporting)`)

saveRDS(table_3_out, paste0(diry,"/tables/Table_3.rds"))


# Table 4 -----------------------------------------------------------------
# hunting activity and harvest by hunting district
#

table_4_loc1 <-  NULL
for(dist in unique(harv$zone_location_1)){

  table_4_loc1_tmp <- harv %>%
    ungroup() %>%
    mutate(active1 = ifelse(zone_location_1 == dist,
                            active1,"N"),
           success_kill1 = ifelse(zone_location_1 == dist,
                                  success_kill1,"N"),
           tech_days_max1 = ifelse(zone_location_1 == dist,
                                   tech_days_max1,0),
           tech_kill_sum1 = ifelse(zone_location_1 == dist,
                                   tech_kill_sum1,0)
    ) %>%
    group_by(analysis_stratum) %>%
    summarise(active_hunters = prop_func(active1,
                                         success = "Y",
                                         pop = population,
                                         val = "mean"),
              active_hunters_se = prop_func(active1,
                                            success = "Y",
                                            pop = population,
                                            val = "se"),
              success_hunters = prop_func(success_kill1,
                                          success = "Y",
                                          pop = population,
                                          val = "mean"),
              success_hunters_se = prop_func(success_kill1,
                                             success = "Y",
                                             pop = population,
                                             val = "se"),
              days_hunting = numeric_func(tech_days_max1,
                                          pop = population,
                                          val = "mean"),
              days_hunting_se = numeric_func(tech_days_max1,
                                             pop = population,
                                             val = "se"),
              harvest = numeric_func(tech_kill_sum1,
                                     pop = population,
                                     val = "mean"),
              harvest_se = numeric_func(tech_kill_sum1,
                                        pop = population,
                                        val = "se"),
              mean_kill = mean(tech_kill_sum1,na.rm = TRUE),
              max_kill = max(tech_kill_sum1,na.rm = TRUE),
              median_kill = median(tech_kill_sum1)) %>%
    mutate(district = dist)


  table_4_loc1 <- bind_rows(table_4_loc1,table_4_loc1_tmp)
}

table_4_loc1_tot <- table_4_loc1 %>%
  group_by(district) %>%
  summarise(active_hunters = sum(active_hunters),
            active_hunters_se = round(sqrt(sum(active_hunters_se^2))),
            success_hunters = sum(success_hunters),
            success_hunters_se = round(sqrt(sum(success_hunters_se^2))),
            days_hunting = sum(days_hunting),
            days_hunting_se = round(sqrt(sum(days_hunting_se^2))),
            harvest = sum(harvest),
            harvest_se = round(sqrt(sum(harvest_se^2)))) %>%
  filter(!is.na(district))






table_4_loc2 <-  NULL
for(dist in unique(harv$zone_location_2)){

  table_4_loc2_tmp <- harv %>%
    ungroup() %>%
    mutate(active2 = ifelse(zone_location_2 == dist,
                            active2,"N"),
           success_kill2 = ifelse(zone_location_2 == dist,
                                  success_kill2,"N"),
           tech_days_max2 = ifelse(zone_location_2 == dist,
                                   tech_days_max2,0),
           tech_kill_sum2 = ifelse(zone_location_2 == dist,
                                   tech_kill_sum2,0)
    ) %>%
    group_by(analysis_stratum) %>%
    summarise(active_hunters = prop_func(active2,
                                         success = "Y",
                                         pop = population,
                                         val = "mean"),
              active_hunters_se = prop_func(active2,
                                            success = "Y",
                                            pop = population,
                                            val = "se"),
              success_hunters = prop_func(success_kill2,
                                          success = "Y",
                                          pop = population,
                                          val = "mean"),
              success_hunters_se = prop_func(success_kill2,
                                             success = "Y",
                                             pop = population,
                                             val = "se"),
              days_hunting = numeric_func(tech_days_max2,
                                          pop = population,
                                          val = "mean"),
              days_hunting_se = numeric_func(tech_days_max2,
                                             pop = population,
                                             val = "se"),
              harvest = numeric_func(tech_kill_sum2,
                                     pop = population,
                                     val = "mean"),
              harvest_se = numeric_func(tech_kill_sum2,
                                        pop = population,
                                        val = "se"),
              mean_kill = mean(tech_kill_sum2,na.rm = TRUE),
              max_kill = max(tech_kill_sum2,na.rm = TRUE),
              median_kill = median(tech_kill_sum2)) %>%
    mutate(district = dist)


  table_4_loc2 <- bind_rows(table_4_loc2,table_4_loc2_tmp)
}

table_4_loc2_tot <- table_4_loc2 %>%
  group_by(district) %>%
  summarise(active_hunters = sum(active_hunters),
            active_hunters_se = round(sqrt(sum(active_hunters_se^2))),
            success_hunters = sum(success_hunters),
            success_hunters_se = round(sqrt(sum(success_hunters_se^2))),
            days_hunting = sum(days_hunting),
            days_hunting_se = round(sqrt(sum(days_hunting_se^2))),
            harvest = sum(harvest),
            harvest_se = round(sqrt(sum(harvest_se^2)))) %>%
  filter(!is.na(district))






table_4_loc3 <-  NULL
for(dist in unique(harv$zone_location_3)){

  table_4_loc3_tmp <- harv %>%
    ungroup() %>%
    mutate(active3 = ifelse(zone_location_3 == dist,
                            active3,"N"),
           success_kill3 = ifelse(zone_location_3 == dist,
                                  success_kill3,"N"),
           tech_days_max3 = ifelse(zone_location_3 == dist,
                                   tech_days_max3,0),
           tech_kill_sum3 = ifelse(zone_location_3 == dist,
                                   tech_kill_sum3,0)
    ) %>%
    group_by(analysis_stratum) %>%
    summarise(active_hunters = prop_func(active3,
                                         success = "Y",
                                         pop = population,
                                         val = "mean"),
              active_hunters_se = prop_func(active3,
                                            success = "Y",
                                            pop = population,
                                            val = "se"),
              success_hunters = prop_func(success_kill3,
                                          success = "Y",
                                          pop = population,
                                          val = "mean"),
              success_hunters_se = prop_func(success_kill3,
                                             success = "Y",
                                             pop = population,
                                             val = "se"),
              days_hunting = numeric_func(tech_days_max3,
                                          pop = population,
                                          val = "mean"),
              days_hunting_se = numeric_func(tech_days_max3,
                                             pop = population,
                                             val = "se"),
              harvest = numeric_func(tech_kill_sum3,
                                     pop = population,
                                     val = "mean"),
              harvest_se = numeric_func(tech_kill_sum3,
                                        pop = population,
                                        val = "se"),
              mean_kill = mean(tech_kill_sum3,na.rm = TRUE),
              max_kill = max(tech_kill_sum3,na.rm = TRUE),
              median_kill = median(tech_kill_sum3)) %>%
    mutate(district = dist)


  table_4_loc3 <- bind_rows(table_4_loc3,table_4_loc3_tmp)
}

table_4_loc3_tot <- table_4_loc3 %>%
  group_by(district) %>%
  summarise(active_hunters = sum(active_hunters),
            active_hunters_se = round(sqrt(sum(active_hunters_se^2))),
            success_hunters = sum(success_hunters),
            success_hunters_se = round(sqrt(sum(success_hunters_se^2))),
            days_hunting = sum(days_hunting),
            days_hunting_se = round(sqrt(sum(days_hunting_se^2))),
            harvest = sum(harvest),
            harvest_se = round(sqrt(sum(harvest_se^2)))) %>%
  filter(!is.na(district))




table_4 <- table_4_loc1_tot %>%
  bind_rows(table_4_loc2_tot) %>%
  bind_rows(table_4_loc3_tot) %>%
  group_by(district) %>%
  summarise(active_hunters = sum(active_hunters,na.rm = TRUE),
            active_hunters_se = round(sqrt(sum(active_hunters_se^2,na.rm = TRUE))),
            success_hunters = sum(success_hunters,na.rm = TRUE),
            success_hunters_se = round(sqrt(sum(success_hunters_se^2,na.rm = TRUE))),
            days_hunting = sum(days_hunting,na.rm = TRUE),
            days_hunting_se = round(sqrt(sum(days_hunting_se^2,na.rm = TRUE))),
            harvest = sum(harvest,na.rm = TRUE),
            harvest_se = round(sqrt(sum(harvest_se^2,na.rm = TRUE)))) %>%
  mutate(proportional_distribution_of_harvest = round(harvest/sum(harvest,na.rm = TRUE),2))



district_names <- tibble(district = factor(c(sort(unique(table_4$district)),"T"),
                                           ordered = TRUE),
                         `District of Hunt` = c(sort(unique(table_4$district))[-length(sort(unique(table_4$district)))],"Ontario","Total"))

table_4_out <- district_names %>%
  inner_join(table_4, by = "district") %>%
  mutate(`Estimated Number Active` = paste0(active_hunters," ± ",active_hunters_se),
         `Estimated Number Successful` = paste0(success_hunters," ± ",success_hunters_se),
         `Estimated Number of Days` = paste0(days_hunting," ± ",days_hunting_se),
         `Estimated Harvest` = paste0(harvest," ± ",harvest_se),
         `Proportional Distribution of Harvest` = proportional_distribution_of_harvest) %>%
  select(`District of Hunt`,
         `Estimated Number Active`,
         `Estimated Number Successful`,
         `Estimated Number of Days`,
         `Estimated Harvest`,
         `Proportional Distribution of Harvest`)



activ_pops_district <- table_4 %>%
  select(district,active_hunters) %>%
  rename(active_hunter_pop_district = active_hunters)


harv <- harv %>%
  left_join(activ_pops_district,
            by = c("zone_location_1" = "district"))



saveRDS(table_4_out, paste0(diry,"/tables/Table_4.rds"))



# Table 5 -----------------------------------------------------------------






table5 <- harv %>%
  ungroup() %>%
  group_by(analysis_stratum) %>%
  summarise(hunters_used_recordings = prop_func(used_recordings,
                                                success = "Y",
                                                pop = population,
                                                val = "mean"),
            hunters_used_recordings_se = prop_func(used_recordings,
                                                   success = "Y",
                                                   pop = population,
                                                   val = "se"),
            hunters_used_guide = prop_func(used_guide,
                                           success = "Y",
                                           pop = population,
                                           val = "mean"),
            hunters_used_guide_se = prop_func(used_guide,
                                              success = "Y",
                                              pop = population,
                                              val = "se"),
            spring_hunters_in_fall = prop_func(hunted_in_fall,
                                               success = "Y",
                                               pop = population,
                                               val = "mean"),
            spring_hunters_in_fall_se = prop_func(hunted_in_fall,
                                                  success = "Y",
                                                  pop = population,
                                                  val = "se"),
            n_active_hunters = unique(active_hunters_pop)) %>%
  mutate(hunters_used_recordings_p_active = round(hunters_used_recordings/n_active_hunters,2),
         hunters_used_guide_p_active = round(hunters_used_guide/n_active_hunters,2),
         spring_hunters_in_fall_p_active = round(spring_hunters_in_fall/n_active_hunters,2))










total_active_hunters <- table_2 %>%
  filter(analysis_stratum == "T") %>%
  select(active_hunters) %>%
  unlist() %>%
  as.integer()






table5_tot <- table5 %>%
  ungroup() %>%
  summarise(hunters_used_recordings = sum(hunters_used_recordings),
            hunters_used_recordings_se = round(sqrt(sum(hunters_used_recordings_se^2))),
            hunters_used_guide = sum(hunters_used_guide),
            hunters_used_guide_se = round(sqrt(sum(hunters_used_guide_se^2))),
            spring_hunters_in_fall = sum(spring_hunters_in_fall),
            spring_hunters_in_fall_se = round(sqrt(sum(spring_hunters_in_fall_se^2)))) %>%
  mutate(analysis_stratum = "T",
         hunters_used_recordings_p_active = round(hunters_used_recordings/total_active_hunters,2),
         hunters_used_guide_p_active = round(hunters_used_guide/total_active_hunters,2),
         spring_hunters_in_fall_p_active = round(spring_hunters_in_fall/total_active_hunters,2))

table_5 <- table5 %>%
  bind_rows(table5_tot)



table_5_out <- strata_names %>%
  inner_join(table_5, by = "analysis_stratum") %>%
  mutate(`Estimated Number of Hunters Using Recordings` = paste0(hunters_used_recordings," ± ",hunters_used_recordings_se),
         `Proportion of Active Hunters Using Recordings` = hunters_used_recordings_p_active,
         `Estimated Number of Hunters Using Guide` = paste0(hunters_used_guide," ± ",hunters_used_guide_se),
         `Proportion of Active Hunters Using Guide` = hunters_used_guide_p_active,
         `Estimated Number of Spring Hunters Also Hunting Snow Geese in the Fall` = paste0(spring_hunters_in_fall," ± ",spring_hunters_in_fall_se),
         `Proportion of Active Hunters Also Hunting Snow Geese in the Fall` = spring_hunters_in_fall_p_active) %>%
  select(`Stratum`,
         `Estimated Number of Hunters Using Recordings`,
         `Proportion of Active Hunters Using Recordings`,
         `Estimated Number of Hunters Using Guide`,
         `Proportion of Active Hunters Using Guide`,
         `Estimated Number of Spring Hunters Also Hunting Snow Geese in the Fall`,
         `Proportion of Active Hunters Also Hunting Snow Geese in the Fall`)


saveRDS(table_5_out, paste0(diry,"/tables/Table_5.rds"))





# Table 6 -----------------------------------------------------------------

table6 <- harv %>%
  filter(reported_tech == "Y") %>%
  group_by(zone_location_1) %>%
  summarise(n_rep_tech = n(),
            n_used_blind = prop_func(used_blind,
                                     pop = active_hunter_pop_district,
                                     val = "mean"),
            n_used_pass = prop_func(used_pass,
                                    pop = active_hunter_pop_district,
                                    val = "mean"),
            n_used_stalking = prop_func(used_stalking,
                                        pop = active_hunter_pop_district,
                                        val = "mean")) %>%
  left_join(activ_pops_district,
            by = c("zone_location_1" = "district")) %>%
  mutate(p_used_blind = 100*round(n_used_blind/active_hunter_pop_district,2),
         p_used_pass = 100*round(n_used_pass/active_hunter_pop_district,2),
         p_used_stalking = 100*round(n_used_stalking/active_hunter_pop_district,2))

table6_tot <- table6 %>%
  summarise(n_rep_tech = sum(n_rep_tech),
            n_used_blind = sum(n_used_blind),
            n_used_pass = sum(n_used_pass),
            n_used_stalking = sum(n_used_stalking)) %>%
  mutate(zone_location_1 = "T",
         p_used_blind = 100*round(n_used_blind/total_active_hunters,2),
         p_used_pass = 100*round(n_used_pass/total_active_hunters,2),
         p_used_stalking = 100*round(n_used_stalking/total_active_hunters,2))




table_6 <- table6 %>%
  bind_rows(table6_tot) %>%
  rename(district = zone_location_1) %>%
  relocate(district,
           n_rep_tech,
           n_used_blind,
           p_used_blind,
           n_used_pass,
           p_used_pass,
           n_used_stalking,
           p_used_stalking) %>%
  select(-active_hunter_pop_district)



table_6_out <- district_names %>%
  inner_join(table_6, by = "district") %>%
  mutate(`Number Reporting Hunting Technique` = n_rep_tech,
         `Estimated Number of Hunters Using Blinds and Decoys (% of Active)` = paste0(n_used_blind," (",p_used_blind,")"),
         `Estimated Number of Hunters Using Pass Shooting (% of Active)` = paste0(n_used_pass," (",p_used_pass,")"),
         `Estimated Number of Hunters Using Stalking (% of Active)` = paste0(n_used_stalking," (",p_used_stalking,")")) %>%
  select(`District of Hunt`,
         `Number Reporting Hunting Technique`,
         `Estimated Number of Hunters Using Blinds and Decoys (% of Active)`,
         `Estimated Number of Hunters Using Pass Shooting (% of Active)`,
         `Estimated Number of Hunters Using Stalking (% of Active)`)


saveRDS(table_6_out, paste0(diry,"/tables/Table_6.rds"))



# Table 7 -----------------------------------------------------------------



table7 <- harv %>%
  filter(reported_tech == "Y") %>%
  group_by(zone_location_1) %>%
  summarise(n_rep_tech = n(),
            n_days_blind = numeric_func(days_blind,
                                        pop = active_hunter_pop_district,
                                        val = "mean"),
            n_days_pass = numeric_func(days_pass,
                                       pop = active_hunter_pop_district,
                                       val = "mean"),
            n_days_stalking = numeric_func(days_stalking,
                                           pop = active_hunter_pop_district,
                                           val = "mean"),
            mean_days_blind = round(mean(days_blind),2),
            mean_days_pass = round(mean(days_pass),2),
            mean_days_stalking = round(mean(days_stalking),2)) %>%
  left_join(activ_pops_district,
            by = c("zone_location_1" = "district"))

table7_tot <- table7 %>%
  summarise(n_rep_tech = sum(n_rep_tech),
            n_days_blind = sum(n_days_blind),
            n_days_pass = sum(n_days_pass),
            n_days_stalking = sum(n_days_stalking)) %>%
  mutate(zone_location_1 = "T")

table_7 <- table7 %>%
  bind_rows(table7_tot) %>%
  rename(district = zone_location_1) %>%
  relocate(district,
           n_rep_tech,
           n_days_blind,
           mean_days_blind,
           n_days_pass,
           mean_days_pass,
           n_days_stalking,
           mean_days_stalking) %>%
  select(-active_hunter_pop_district)



table_7_out <- district_names %>%
  inner_join(table_7, by = "district") %>%
  mutate(`Number Reporting Hunting Technique and Days` = n_rep_tech,
         `Estimated Number of Days Using Blinds and Decoys (Days/Active Hunter)` = paste0(n_days_blind," (",mean_days_blind,")"),
         `Estimated Number of Days Using Pass Shooting (Days/Active Hunter)` = paste0(n_days_pass," (",mean_days_pass,")"),
         `Estimated Number of Days Using Stalking (Days/Active Hunter)` = paste0(n_days_stalking," (",mean_days_stalking,")"),
         across(starts_with("Estimated"),.fns = ~gsub("(NA)","",.x,fixed = TRUE))) %>%
  select(`District of Hunt`,
         `Number Reporting Hunting Technique and Days`,
         `Estimated Number of Days Using Blinds and Decoys (Days/Active Hunter)`,
         `Estimated Number of Days Using Pass Shooting (Days/Active Hunter)`,
         `Estimated Number of Days Using Stalking (Days/Active Hunter)`)


saveRDS(table_7_out, paste0(diry,"/tables/Table_7.rds"))


# Table 8 -----------------------------------------------------------------




table8 <- harv %>%
  filter(reported_tech == "Y") %>%
  group_by(zone_location_1) %>%
  summarise(n_rep_tech = n(),
            n_killed_blind = numeric_func(killed_blind,
                                          pop = active_hunter_pop_district,
                                          val = "mean"),
            n_killed_pass = numeric_func(killed_pass,
                                         pop = active_hunter_pop_district,
                                         val = "mean"),
            n_killed_stalking = numeric_func(killed_stalking,
                                             pop = active_hunter_pop_district,
                                             val = "mean"),
            mean_killed_blind = round(mean(killed_blind),2),
            mean_killed_pass = round(mean(killed_pass),2),
            mean_killed_stalking = round(mean(killed_stalking),2)) %>%
  left_join(activ_pops_district,
            by = c("zone_location_1" = "district"))

table8_tot <- table8 %>%
  summarise(n_rep_tech = sum(n_rep_tech),
            n_killed_blind = sum(n_killed_blind),
            n_killed_pass = sum(n_killed_pass),
            n_killed_stalking = sum(n_killed_stalking)) %>%
  mutate(zone_location_1 = "T")

table_8 <- table8 %>%
  bind_rows(table8_tot) %>%
  rename(district = zone_location_1) %>%
  relocate(district,
           n_rep_tech,
           n_killed_blind,
           mean_killed_blind,
           n_killed_pass,
           mean_killed_pass,
           n_killed_stalking,
           mean_killed_stalking) %>%
  select(-active_hunter_pop_district)



table_8_out <- district_names %>%
  inner_join(table_8, by = "district") %>%
  mutate(`Number Reporting Hunting Technique and Harvest` = n_rep_tech,
         `Estimated Harvest Using Blinds and Decoys (Harvest/Active Hunter)` = paste0(n_killed_blind," (",mean_killed_blind,")"),
         `Estimated Harvest Using Pass Shooting (Harvest/Active Hunter)` = paste0(n_killed_pass," (",mean_killed_pass,")"),
         `Estimated Harvest Using Stalking (Harvest/Active Hunter)` = paste0(n_killed_stalking," (",mean_killed_stalking,")"),
         across(starts_with("Estimated"),.fns = ~gsub("(NA)","",.x,fixed = TRUE))) %>%
  select(`District of Hunt`,
         `Number Reporting Hunting Technique and Harvest`,
         `Estimated Harvest Using Blinds and Decoys (Harvest/Active Hunter)`,
         `Estimated Harvest Using Pass Shooting (Harvest/Active Hunter)`,
         `Estimated Harvest Using Stalking (Harvest/Active Hunter)`)




saveRDS(table_8_out, paste0(diry,"/tables/Table_8.rds"))




















# Table 9 -----------------------------------------------------------------


##  calculate the mean kill per day for hunters in each zone
##  calculate the ratio of the total harvest to the mean kill per day (days)
##  multiply that ratio by the proportion of the hunters reporting each bag



harv_summary <- table_2 %>%
  select(analysis_stratum,harvest) %>%
  filter(analysis_stratum != "T") %>%
  rename(harv = harvest)

permit_by_district <- harv %>%
  select(id,analysis_stratum) %>%
  distinct()


kpd_by_strat <- bags %>%
  inner_join(permit_by_district,
             by = "id") %>%
  group_by(analysis_stratum) %>%
  summarise(kpd = mean(killed),
            tot_count = n()) %>%
  full_join(harv_summary,
             by = "analysis_stratum") %>%
  mutate(kpd = ifelse(harv == 0,
                      0,kpd),
         tot_count = ifelse(harv == 0,
                            0,tot_count)) |>
  mutate(factor = harv/kpd,
         factor = ifelse(is.na(factor),
                         1,factor))



rep_bags_strat <- bags %>%
  inner_join(permit_by_district,
             by = "id") %>%
  group_by(analysis_stratum,killed) %>%
  summarise(count = n()) %>%
  full_join(harv_summary,
            by = "analysis_stratum") |>
  select(-harv) |>
  mutate(count = ifelse(is.na(count),0,count),
         killed = ifelse(is.na(killed),0,killed)) |>
  left_join(kpd_by_strat,by = "analysis_stratum") %>%
  mutate(prop = count/tot_count,
         estdb = factor*prop) %>%
  select(analysis_stratum,killed,count,kpd,
         tot_count,harv,factor,prop,estdb) %>%
  arrange(killed)

rem_zero <- function(x){
  y <- as.character(x)
  for(i in 1:length(y)){
    y[i] <- ifelse(y[i] == "0",
                   "",y[i])
  }
return(y)
}

repl_nan <- function(x){
  y <- x
  for(i in 1:length(y)){
  y[i] <- ifelse(y[i] == "NaN",
                 "",y[i])

  }
  return(y)
}


table_9 <- rep_bags_strat %>%
  select(killed,analysis_stratum,estdb) %>%
  arrange(analysis_stratum,killed) %>%
  rename(daily_bag = killed) %>%
  pivot_wider(id_cols = daily_bag,
              names_from = c(analysis_stratum),
              values_from = estdb,
              values_fill = 0) %>%
  arrange(daily_bag) %>%
  rowwise() %>%
  mutate(across(D:S,.fns = ~round(.x)),
         total = sum(c_across(D:S),na.rm = TRUE),
         harvest = total*daily_bag,
         across(D:S,.fns = ~rem_zero(.x)),
         across(D:S,.fns = ~repl_nan(.x))) %>%
  rename(`Daily Bag` = daily_bag,
         `Quebec D` = D,
         `Quebec E` = E,
         `Quebec F` = F,
         `Quebec G` = G,
         `Quebec H` = H,
         `Quebec I` = I,
         `Ontario WMU 65` = O,
         `Ontario Outside WMU 65` = P,
         `Spring Permits` = S,
         Total = total
  )

print(paste("Total goose harvest is",round(sum(harv_summary$harv)),
            "Total harvest by daily bag is",sum(table_9$harvest)))

table_9_out <- table_9 %>%
  select(-harvest)


saveRDS(table_9_out, paste0(diry,"/tables/Table_9.rds"))



















# Seasonal pattern in harvest and activity --------------------------------



calendars <- readxl::read_xls(paste0(diry,"/Ques4.xls"),
                              col_types = c(rep("numeric",4))) %>%
  rename_with(.,.fn = str_to_lower) %>%
  mutate(date = ymd(paste(y,month,day,sep = "/")),
         killed = ifelse(killed < 0,NA,killed),
         killed = ifelse(killed > 50,50,killed)) %>%
  filter(!is.na(date),
         !is.na(killed)) %>%
  #select(id,killed,date) %>%
  arrange(date)

daily_harv <- calendars %>%
  group_by(date) %>%
  summarise(day_harv = sum(killed),
            day_activ = n()) %>%
  mutate(prop_harv = day_harv/sum(day_harv),
         prop_activ = day_activ/sum(day_activ))

seasonal_pattern <- data.frame(date = as_date(min(daily_harv$date):max(daily_harv$date))) %>%
  left_join(daily_harv, by = "date") %>%
  mutate(across(c(day_harv:prop_activ),
                .fn = ~ifelse(is.na(.x),0,.x)),
         wkday = wday(date)) %>%
  select(-c(day_harv,day_activ)) %>%
  pivot_longer(cols = c(prop_harv,prop_activ),
               names_to = "harv_active",
               values_to = "proportion") %>%
  mutate(metric = ifelse(harv_active == "prop_harv",
                         "Harvest","Activity")) %>%
  select(-harv_active)




# table 10 ----------------------------------------------------------------

add_row_table10 <- FALSE
if(add_row_table10){
  tab10 <- read_csv(paste0("Eastern/table10_historical.csv"))


  tab10_sub <- table_2 %>%
    filter(!(analysis_stratum %in% c("T","S"))) %>%
    ungroup() %>%
    summarise(active_hunters = sum(active_hunters),
              active_hunters_se = round(sqrt(sum(active_hunters_se^2))),
              success_hunters = sum(success_hunters),
              success_hunters_se = round(sqrt(sum(success_hunters_se^2))),
              days_hunting = sum(days_hunting),
              days_hunting_se = round(sqrt(sum(days_hunting_se^2))),
              harvest = sum(harvest),
              harvest_se = round(sqrt(sum(harvest_se^2)))) %>%
    mutate(Year = y,
           `Estimated Number Active` = paste0(active_hunters," ± ",active_hunters_se),
           `Estimated Number Successful` = paste0(success_hunters," ± ",success_hunters_se),
           `Estimated Number of Days` = paste0(days_hunting," ± ",days_hunting_se),
           `Estimated Harvest` = paste0(harvest," ± ",harvest_se)) %>%
    select(Year,
           `Estimated Number Active`,
           `Estimated Number Successful`,
           `Estimated Number of Days`,
           `Estimated Harvest`)


  table_10_out <- tab10 %>%
    bind_rows(tab10_sub) %>%
    arrange(-Year)

  write_excel_csv(table_10_out,paste0("Eastern/table10_historical.csv"))

  saveRDS(table_10_out, paste0(diry,"/tables/Table_10.rds"))


}

resp_rate <- table_1 %>%
  filter(analysis_stratum == "T") %>%
  select(prop_response) %>%
  unlist() %>%
  unname()

p_hunt_rep_GT_1_district <- table_3 %>%
  filter(analysis_stratum == "T") %>%
  select(p_hunt_GT_1_district) %>%
  unlist() %>%
  unname()

p_harvest_ontario <- table_4 %>%
  filter(district == "O") %>%
  select(proportional_distribution_of_harvest) %>%
  unlist() %>%
  unname()


save(list = c("table_1_out",
              "table_2_out",
              "table_3_out",
              "table_4_out",
              "table_5_out",
              "table_6_out",
              "table_7_out",
              "table_8_out",
              "table_9_out",
              "table_10_out",
              "resp_rate",
              "p_hunt_rep_GT_1_district",
              "p_harvest_ontario",
              "seasonal_pattern"),
     file = paste0(diry,"/all_tables.RData"))
