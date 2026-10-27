#### full Western Analysis for Spring Snow Goose Survey
####
#### Run this file in late summer/early fall once the survey responses are
#### available.
#### This file creates the output files, tables, etc. necessary to support the
#### report generation in the SK_AB_Spring_Goose_Harvest.qmd
####


library(tidyverse)

y = 2026

ly = y-1

prov_sel <- c("AB","SK")

permit_file_dir <- "Permits 2025 (as of Sept 25 2026).xlsx"

abregsk <- read.table(paste0("Western/protected/SSGS ",y," SK.txt")) %>%
  rename(permit = 1) %>%
  mutate(id = (y-1)*1000000+permit,
         prov = "SK")

abregab <- read.table(paste0("Western/protected/SSGS ",y," AB.txt")) %>%
  rename(permit = 1) %>%
  mutate(id = (y-1)*1000000+permit,
         prov = "AB")

abreg <- bind_rows(abregsk,abregab)
regsamp <- as.integer(abreg[,"permit"])

final_permits <- readxl::read_xlsx(paste0("data/protected/",permit_file_dir)) %>%
  rename_with(., .fn = str_to_lower) %>%
  rename(permit_type = 'permit type') %>%
  mutate(permit = as.integer(permit),
         id = (y-1)*1000000+permit)

if(length(which(is.na(final_permits$id))) > 0){
  stop("Some rows of permit file have no id - permit number")
}

spring <- final_permits %>%
  filter(grepl("pring",permit_type)) |>
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
  filter(analysis_province %in% prov_sel) %>%
  mutate(sampled = ifelse(!grepl("not sample",permit_type,fixed = TRUE),
                           TRUE,FALSE),
         season = "spring")


# for regular season, the residence is the most relevant location information
regular_season <- final_permits %>%
  filter(grepl("egular",permit_type)) |>
  mutate(analysis_province = ifelse(!is.na(province),
                                    province, sampling_prov),
         analysis_province = ifelse(is.na(analysis_province),
                                    hunting_province, analysis_province), # prov is hunting prov, then sampling prov, then residence
         analysis_lat = ifelse(!is.na(residence_latitude),
                               residence_latitude,salelat),
         analysis_lat = ifelse(is.na(analysis_lat),
                               hunting_latitude,analysis_lat),
         analysis_long = ifelse(!is.na(residence_longitude),
                                residence_longitude,hunting_longitude),
         analysis_long = ifelse(is.na(analysis_long),
                                salelon,analysis_long))%>%
  filter(analysis_province %in% prov_sel) %>%
  mutate(sampled = ifelse(id %in% abreg$id,
                           TRUE,FALSE),
         season = "regular")

if(table(regular_season$sampled)["TRUE"] != nrow(abreg)){

  missing_sampled_permits <- abreg |>
    filter(!id %in% regular_season$id)

  prov_miss <- table(missing_sampled_permits$prov)

  stop(paste(nrow(missing_sampled_permits),"sampled permits are missing from the permit file",
             "including",prov_miss["AB"],"from AB and",prov_miss["SK"],"from SK."))

  write_csv(missing_sampled_permits,paste("missing_sampled_permits_west",y,".csv"))
}


strat_join <- data.frame(strat = c("Spring-AB",
                                   "Spring-SK",
                                   "Spring-US",
                                   "Regular-US",
                                   "Regular-AB",
                                   "Regular-SK"),
                         stratum = c("ZA",
                                     "ZS",
                                     "ZU",
                                     "US",
                                     "AB",
                                     "SK"))

full_permits <- bind_rows(spring,regular_season) %>%
  mutate(id = (ly*1000000) + permit,
         strat = paste(str_to_sentence(season),analysis_province,sep = "-"),
         strat = ifelse(!is.na(state),
                        paste(str_to_sentence(season),"US",sep = "-"),
                        strat)) %>%
  left_join(strat_join,
            by = "strat")


full_sampled <- full_permits %>%
  filter(sampled)

pops <- full_permits %>%
  group_by(stratum,season) %>%
  summarise(population = n(),
            .groups = "keep")


sampled_pops <- full_sampled %>%
  group_by(stratum,season) %>%
  summarise(sample = n(),
            .groups = "keep")

# sampling component of table 1 see line 623
tab1_samp <- inner_join(pops,sampled_pops,
                   by = c("season","stratum")) %>%
  arrange(season,stratum)


westsamp <- full_sampled %>%
  select(id,season,stratum)

write_excel_csv(westsamp,"Western/protected/all western sampled permits.csv")

# Responses ---------------------------------------------------------------

q1 <- readxl::read_xlsx("Western/protected/Ques1_3.xlsx",
                       col_types = c("numeric",
                                     "text",
                                     rep("numeric",4))) %>%
  rename(id = ID,
         active = Q1,
         days = Q2,
         snow = Q3,
         ross = Q4,
         total = Q5) %>%
  mutate(active = str_to_upper(active))







# Calendar summary and plotting -------------------------------------------


bags <- readxl::read_xlsx(paste0("Western/protected/Ques4.xlsx"),
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
  mutate(total = ifelse(cal_kil_sum > total,
                       cal_kil_sum,total),
         days = ifelse(cal_days_sum > days,
                       cal_days_sum,days))


plot(resp_combined$total,resp_combined$cal_kil_sum)
table(resp_combined$total,resp_combined$cal_kil_sum, useNA = "always")
plot(resp_combined$days,resp_combined$cal_days_sum)






numerate <- function(x){
  seq(1,length((x)))
}

technique <- readxl::read_xlsx("Western/protected/Ques5.xlsx",
                              col_types = c("numeric",
                                            "text",
                                            rep("numeric",6)))%>%
  rename_with(.,.fn = str_to_lower) %>%
  filter(!is.na(id)) %>%
  arrange(id) %>%
  mutate(loc_n = numerate(id),
         .by = id) %>%
  mutate(town = str_extract(locality,".+(?=,)"),
         prov = str_extract(locality,"(?<=,)[[:blank:]][[:graph:]]+"),
         prov = str_trim(ifelse(is.na(prov),
                       str_extract(locality,"[[:upper:]]{2}$"),
                       prov)),
         prov = ifelse(prov == "NWT","NT",prov),
         prov = ifelse(prov == "Alberta","AB",prov),
         prov = ifelse(prov == "Saskatchewan","SK",prov),
         town = ifelse(is.na(town),
                       locality,town),
         town = str_to_lower(town))

miss_towns <- technique %>%
  filter(!prov %in% prov_sel,
         !prov %in% c("BC","MB","NB","NL","NT",
                      "NS","NU","ON","PE","QC","YT"),
         !is.na(prov))

# location information for fixing missing towns ---------------------------


if(nrow(miss_towns) > 0){
can_town_names <- sf::read_sf("gis/cgn_canada_shp_eng.shp")

provs <- can_town_names %>%
  sf::st_drop_geometry() %>%
  select(PROV_TERR) %>%
  filter(PROV_TERR != "Undersea Feature") %>%
  distinct() %>%
  arrange(PROV_TERR) %>%
  mutate(prov = c("AB","BC","MB","NB","NL","NT",
                  "NS","NU","ON","PE","QC","SK","YT")) %>%
  filter(prov %in% prov_sel) %>%
  mutate(prov = factor(prov,levels = c("AB","BC","MB","NB","NL","NT",
                                       "NS","NU","ON","PE","QC","SK","YT","Total"),
                       ordered = TRUE))

totprov <- provs[1,]
totprov <- totprov %>%
  mutate(PROV_TERR = "Total",
         prov = factor("Total",
                       levels = c("AB","BC","MB","NB","NL","NT",
                                  "NS","NU","ON","PE","QC","SK","YT","Total"),
                       ordered = TRUE))
provs <- provs %>%
  bind_rows(.,totprov) %>%
  mutate(PROV_TERR = factor(PROV_TERR),
         PROV_TERR = fct_reorder(PROV_TERR,
                                 .x = as.integer(prov)))

where_towns <- can_town_names %>%
  mutate(town = str_to_lower(GEONAME)) %>%
  filter(town %in% miss_towns$town,
         PROV_TERR %in% provs$PROV_TERR) %>%
  select(town,PROV_TERR,geometry) %>%
  left_join(provs,by = "PROV_TERR")

fix_towns <- where_towns %>%
  sf::st_drop_geometry() %>%
  as.data.frame()
for(j in 1:nrow(fix_towns)){
  p <- as.character(fix_towns[j,"prov"])
  tn <- as.character(fix_towns[j,"town"])

  technique[which(technique$town == tn),"prov"] <- p



}







basemap <- bbsBayes2::load_map("prov_state") %>%
  sf::st_transform(.,crs = sf::st_crs(where_towns))
bb <- sf::st_bbox(where_towns)

miss_town_plot <- ggplot()+
  geom_sf(data = basemap)+
  geom_sf(data = where_towns)+
  geom_sf_text(data = where_towns,aes(label = town, colour = PROV_TERR),
               size = 4)+
  coord_sf(xlim = bb[c("xmin","xmax")],
           ylim = bb[c("ymin","ymax")])

miss_town_plot

}# end of province repair, if miss_towns > 0 rows

technique <- technique %>%
  filter(prov %in% prov_sel)

reported_tech <- technique %>%
  select(id) %>%
  distinct() %>%
  mutate(reported_tech = "Y")


tech_join <- technique %>%
  pivot_wider(values_from = c(days_pass : killed_blind_decoy,prov),
              id_cols = id,
              names_from = loc_n,
              names_prefix = "location_") %>%
  left_join(reported_tech)



q6 <- readxl::read_xlsx("Western/protected/Ques6_8.xlsx",
                       col_types = c("numeric",
                                     rep("text",3)))%>%
  rename_with(.,.fn = str_to_lower)%>%
  mutate(used_recordings = str_to_upper(used_recordings),
         used_guide = str_to_upper(used_guide),
         hunted_in_fall = str_to_upper(hunted_in_fall))

comments <- readxl::read_xlsx("Western/protected/Comments.xlsx",
                             col_types = c("numeric",
                                           "text"))%>%
  rename_with(.,.fn = str_to_lower) %>%
  mutate(comments = str_to_lower(comments),
         flag = ifelse(grepl("guide",comments) | grepl("group",comments),
                       TRUE,FALSE))


resp_combined <- resp_combined %>%
  left_join(.,tech_join,
            by = "id") %>%
  left_join(.,q6,
            by ="id") %>%
  left_join(.,comments,
            by = "id") %>%
  relocate(id,
           active,
           days,
           total,
           snow,
           ross,
           used_recordings,
           used_guide,
           hunted_in_fall,
           comments,
           flag,
           cal_days_sum,
           cal_kil_sum)





# Manual review of comments to ID group harvests --------------------------

possible_groups <- comments %>%
  filter(flag)

possible_groups <- possible_groups %>%
  left_join(.,resp_combined,
            by = "id")
possible_groups

## enter manual adjustments here if necessary
# q1_clean[which(q1_clean$id == 2022367474),"snow"] <- 8
# q1_clean[which(q1_clean$id == 2022367474),"total"] <- 8
# q1_clean[which(q1_clean$id == 2022367474),"killed_blind_decoy_location_1"] <- 8

# Manual review of harvest to ID group harvests ---------------------------

flag_group_harvest <- 150 # possible reporting for a group of hunters or a guide

possible_group_harv <- resp_combined %>%
  filter(total > flag_group_harvest | total/days > 50)
possible_group_harv %>% select(all_of(c("id","days","snow","ross","total","comments")))

probable_group_harv <- possible_group_harv %>%
  filter(total/days > 50)
probable_group_harv #review these records for possible data-entry errors and group reporting
# if comments indicate number in group, then divide evenly among number of hunters
# if no comments ask Michel to confirm with paper response
# if not a data entry error and mean daily bag > 50, then drop permit from sample
## enter manual adjustments here


chk <- resp_combined |>
  filter(total > 170)

q1_clean <- resp_combined %>%
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
         days = max(c(cal_days_sum,tech_days_max,days),na.rm = TRUE),
         days = ifelse(is.na(days), # zero filling days
                       0,days),
         total = max(c(total,cal_kil_sum,tech_kill_sum), na.rm = TRUE),
         total = ifelse(is.na(total), # zero filling days
                       0,total),
         snow = ifelse((!is.na(days) | !is.na(total)) & is.na(snow),
                        0,snow),
         ross = ifelse((!is.na(days) | !is.na(total)) & is.na(ross),
                       0,ross),
         total = ifelse(total < sum(snow,ross),
                        sum(snow,ross),total),
         snow = ifelse(snow < total-ross,
                       total-ross,snow),
         ross = ifelse(ross < total-snow,
                       total-snow,ross),
         success_total = ifelse(total > 0,
                                "Y","N"),
         success_snow = ifelse(snow > 0,
                                "Y","N"),
         success_ross = ifelse(ross > 0,
                                "Y","N"),
         active = ifelse(total > 0 | days > 0,
                         "Y",active),
         active = ifelse(days == 0 & !is.na(days),"N",active),
         active = ifelse(is.na(active),"N",active),
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
           total,
           snow,
           ross,
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




# filtering responses to sampled ------------------------------------------


strata <- full_sampled %>%
  select(id,stratum,season)

out_range <- q1_clean %>%
  filter(!id %in% full_sampled$id)
table(out_range$prov_location_1)
## check to see if any of the removed responses are for the region of interest
## if they are, check to make sure they are not sampled in alternate region (e.g., in North)
out_check <- out_range %>% filter(prov_location_1 %in% prov_sel) %>%
  select(id) %>%
  unlist()
### these unsampled permits may indicate a hunter that hunted outside of where they
### were selected, or who responded using the wrong permit number
### this should be very few responses
out_check_perms <- final_permits %>%
  filter(id %in% out_check)



# Final harvest file with permits ids strata etc --------------------------


harv <- q1_clean %>%
  inner_join(strata,by= "id")


drop_no_tech <- which(harv$active == "Y" & is.na(harv$reported_tech))

paste(length(drop_no_tech)," active responses did not indicate techniques")

# strata of each response
ids_strats <- harv %>%
  select(id,stratum)

# province of residency of each resident response
ids_ress <- harv %>%
  select(id,stratum) %>%
  filter(stratum %in% c("AB","SK","ZA","ZS")) %>%
  mutate(residents = ifelse(stratum %in% c("AB","ZA"),
                            "AB","SK"))

# number of hunting locations reported in each prov
hunting_locations_region <- technique %>%
  filter(id %in% full_sampled$id) %>%
  group_by(prov) %>%
  summarise(number_hunters = n()) %>%
  arrange(prov,-number_hunters)

# number of residents reporting technique
ids_resident_rep_tech <- technique %>%
  filter(id %in% full_sampled$id) %>%
  inner_join(ids_ress,by = "id")
n_ids_res <- length(unique(ids_resident_rep_tech$id))

hunting_locations_summary <- technique %>%
  filter(id %in% full_sampled$id) %>%
  inner_join(ids_ress,by = "id") %>%
  mutate(travel = ifelse(residents == prov,
                         0,1)) %>%
  group_by(id,residents) %>%
  summarise(number_locations = n(),
            n_prov = length(unique(prov)),
            travel = sum(travel)) %>%
  group_by(residents,number_locations,n_prov,travel) %>%
  summarise(n_ids = n(),.groups = "drop") %>%
  mutate(Province = ifelse(residents == "AB",
                           "AB","SK"))

if(n_ids_res != sum(hunting_locations_summary$n_ids)){
  stop("The number of permits doesn't match")
}

saveRDS(hunting_locations_region,"western/output/reported_hunting_locations.rds")
saveRDS(hunting_locations_summary,"western/output/reported_hunting_loc_summary.rds")


# response rate by stratum and season
resps <- harv %>%
  group_by(season,stratum) %>%
  summarise(responses = n())
# table 1 summary of responses and sampling
tab1 <- tab1_samp %>%
  left_join(.,resps,
            by = c("season","stratum")) %>%
  mutate(responses = ifelse(is.na(responses),0,responses))
# summary lines for table 1 by season
tot1seas <- tab1 %>%
  group_by(season) %>%
  summarise(population = sum(population),
            sample = sum(sample),
            responses = sum(responses)) %>%
  mutate(stratum = "Total")
# overall summary line for table 1
tot1 <- tab1 %>%
  ungroup() %>%
  summarise(population = sum(population),
            sample = sum(sample),
            responses = sum(responses)) %>%
  mutate(stratum = "Total")

tab1 <- tab1 %>%
  bind_rows(.,tot1seas) %>%
  bind_rows(.,tot1) %>%
  mutate(response_rate = 100*round(responses/sample,2))


saveRDS(tab1,"Western/output/table1.rds")
table(harv$active)

saveRDS(harv,"Western/output/harv.rds")

## funtion to calculate proportion of a population that is successful (or active, etc.)
prop_func <- function(x,success = "Y",
                      pop = 1000){
  x <- unlist(x)

  y <- ifelse(!is.na(x),
              x,0)
  y <- ifelse(y == success,1,0)
  m_y = mean(y)*pop
  se_y = sqrt((var(y)*pop^2)/length(x))
  return(list(m_y = m_y,se_y = se_y))
}

# function to calculate the mean and se of x
numeric_func <- function(x,
                      pop = 1000){
  x <- as.numeric(unlist(x))
  y <- ifelse(!is.na(x),
              x,0)
  m_y = mean(y)*pop
  se_y = sqrt((var(y)*pop^2)/length(y))
  return(list(m_y = m_y,se_y = se_y))
}



# Overall Harvest Estimates -----------------------------------------------

vals <- c("active",
          "success_total",
          "days",
          "total",
          "success_snow",
          "snow",
          "success_ross",
          "ross",
          "used_guide",
          "used_recordings",
          "hunted_in_fall")
hdrs <- c("Number of Active Hunters",
          "Number of Successful Hunters",
          "Number of Days",
          "Harvest of All Geese",
          "Successful Hunters (Snow Geese)",
          "Harvest of Snow Geese",
          "Successful Hunters (Ross's Geese)",
          "Harvest of Ross's Geese",
          "Number Using Guide",
          "Number Using Recordings",
          "Number also Hunted in Fall")
vals <- data.frame(val = vals,
                   type = c("prop",
                            "prop",
                            "num",
                            "num",
                            "prop",
                            "num",
                            "prop",
                            "num",
                            "prop",
                            "prop",
                            "prop"),
                   hdr = hdrs)



out <- tab1 %>%
  select(stratum,season)

out_sel <- out %>%
  filter(stratum != "Total")

for(j in 1:nrow(out)){

  st <- out[j,"stratum"] %>% unlist()
  se <- out[j,"season"] %>% unlist()

  if(st != "Total"){
    tmp_pop <- pops %>%
      ungroup() %>%
      filter(stratum == st) %>%
      select(population) %>%
      summarise(pop = sum(population,na.rm = TRUE)) %>%
      unlist() %>%
      as.integer()

    harv_tmp <- harv %>%
      filter(stratum == st)


    for(v in 1:nrow(vals)){
      ty <- vals[v,"type"]
      val <- vals[v,"val"]
      hdr <- vals[v,"hdr"]
      tmp_f <- ifelse(ty == "prop",
                      prop_func,
                      numeric_func)

      if(val == "hunted_in_fall"){
        harv_tmp1 <- harv_tmp %>%
          filter(active == "Y")
        tmp <- tmp_f(harv_tmp1[,val],
                     pop = tmp_pop1)
      }else{
        tmp <- tmp_f(harv_tmp[,val],
                     pop = tmp_pop)
      }
      out[j,hdr] <- paste(round(tmp$m_y),"\U00B1",round(tmp$se_y))
      if(val %in% c("used_guide","used_recordings","hunted_in_fall")){
        out[j,gsub("Number","Proportion of Active Hunters",hdr)] <- round(tmp$m_y/tmp_pop1,2)
      }
      out_sel[j,paste0(hdr,"_est")] <- tmp$m_y
      out_sel[j,paste0(hdr,"_se")] <- tmp$se_y

      if(val == "active"){
        tmp_pop1 <- tmp$m_y
      }
    }


  }else{
    if(is.na(se)){

    sum_tmp <- out_sel %>%
      as.data.frame()

    }else{
      sum_tmp <- out_sel %>%
        filter(season == se) %>%
        as.data.frame()
    }

for(v in 1:nrow(vals)){

  hdr <- vals[v,"hdr"]
 m_y <- sum(as.numeric(sum_tmp[,paste0(hdr,"_est")]))
 se_y <- sqrt(sum(as.numeric(sum_tmp[,paste0(hdr,"_se")])^2))

  out[j,hdr] <- paste(round(m_y),"\U00B1",round(se_y))
  if(hdr %in% c("Number Using Guide","Number Using Recordings",
                "Number also Hunted in Fall")){
    out[j,gsub("Number","Proportion of Active Hunters",hdr)] <- round(m_y/sum(sum_tmp[,"Number of Active Hunters_est"]),2)

  }
}

  }

}


saveRDS(out,"Western/output/simple_harvest_west.rds")

saveRDS(out_sel,"Western/output/simple_harvest_west_raw.rds")

# Estimates by district ---------------------------------------------------






out_d <- data.frame(prov = c("AB","SK"))


for(j in 1:nrow(out_d)){

p <- out_d[j,"prov"]

out_sel_d <- out_sel %>%
  ungroup() %>%
  select(stratum)

for(i in 1:nrow(out_sel_d)){

  st <- as.character(out_sel_d[i,"stratum"])

    tmp_pop <- pops %>%
      ungroup() %>%
      filter(stratum == st) %>%
      select(population) %>%
      summarise(pop = sum(population,na.rm = TRUE)) %>%
      unlist() %>%
      as.integer()




    for(v in 1:nrow(vals)){
      ty <- vals[v,"type"]
      val <- vals[v,"val"]
      hdr <- vals[v,"hdr"]

      harv_tmp <- harv %>%
        filter(stratum == st) %>%
        rename_with(.fn = ~gsub(pattern = val,
                               replacement = "col_sel",
                               x = .x)) %>%
        mutate(col_sel = ifelse((prov_location_1 == p | prov_location_2 == p | prov_location_3 == p),
                      col_sel,
                      NA))



      tmp_f <- ifelse(ty == "prop",
                      prop_func,
                      numeric_func)
      tmp <- tmp_f(harv_tmp[,"col_sel"],
                   pop = tmp_pop)

      out_sel_d[i,paste0(hdr,"_est")] <- tmp$m_y
      out_sel_d[i,paste0(hdr,"_se")] <- tmp$se_y

    }

}

      sum_tmp <- out_sel_d %>%
        as.data.frame()

    for(v in 1:nrow(vals)){

      hdr <- vals[v,"hdr"]
      m_y <- sum(as.numeric(sum_tmp[,paste0(hdr,"_est")]),na.rm = TRUE)
      se_y <- sqrt(sum(as.numeric(sum_tmp[,paste0(hdr,"_se")])^2,na.rm = TRUE))

      out_d[j,hdr] <- paste(round(m_y),"\U00B1",round(se_y))

      if(v == 1){
        out_d[j,"n_active"] <- round(m_y)
      }
      if(v == 3){
        out_d[j,"days"] <- round(m_y)
      }
      if(v == 4){
        out_d[j,"tot_harv"] <- round(m_y)
      }
    }

      out_sel_d <- out_sel_d %>%
        mutate(province = p)
      if(j == 1){
        out_sel_d_sum <- out_sel_d

      }else{
        out_sel_d_sum <- out_sel_d_sum %>%
          bind_rows(out_sel_d)
      }

  }






saveRDS(out_d,"Western/output/district_harvest_west.rds")





# district activity and harvest by technique ------------------------------

techs <- c("pass","stalking","blind_decoy")
val_t <- data.frame(val1 = c("active","days","killed"),
                    val2 = c("days","days","killed"),
                    type = c("prop","prop","prop"),
                    hdr = c("Number of hunters using","Days using","Harvest using"))
locs <- c(paste0("location_",1:3))
vals_t <- expand_grid(tech1 = techs,val_t) %>%
  mutate(val = paste(val2,tech1,sep = "_"),
         hdr = paste(hdr,tech1,sep = " ")) %>%
  select(-val2) %>%
  as.data.frame()



tech_sum_out <- NULL

for(p in c("AB","SK")){


  for(st in as.character(unique(out_sel$stratum))){

    #st <- as.character(out_sel_d[i,"stratum"])

    pop_tmp1 <- out_sel_d_sum %>%
      filter(stratum == st,
             province == p)
    pop_tmp1 <- pop_tmp1[,c("Number of Active Hunters_est",
                            "Number of Days_est",
                            "Harvest of All Geese_est")] %>%
      data.frame()
    names(pop_tmp1) <- c("active","days","killed")

    for(val1 in names(pop_tmp1)){

      harv_tmp <- harv %>%
        filter(stratum == st,
               reported_tech == "Y") %>%
        as.data.frame()



      # harv_tmp[,"col_sel"] <- 0
      #
      # harv_tmp[which(harv_tmp$prov_location_1 == p & !is.na(harv_tmp[,paste0(val,"_location_1")])),
      #          "col_sel"] <- harv_tmp[which(harv_tmp$prov_location_1 == p & !is.na(harv_tmp[,paste0(val,"_location_1")])),
      #                                 paste0(val,"_location_1")] + harv_tmp[which(harv_tmp$prov_location_1 == p & !is.na(harv_tmp[,paste0(val,"_location_1")])),
      #                                                                       "col_sel"]
      # harv_tmp[which(harv_tmp$prov_location_2 == p & !is.na(harv_tmp[,paste0(val,"_location_2")])),
      #          "col_sel"] <- harv_tmp[which(harv_tmp$prov_location_2 == p & !is.na(harv_tmp[,paste0(val,"_location_2")])),
      #                                 paste0(val,"_location_2")] + harv_tmp[which(harv_tmp$prov_location_2 == p & !is.na(harv_tmp[,paste0(val,"_location_2")])),
      #                                                                       "col_sel"]
      # harv_tmp[which(harv_tmp$prov_location_3 == p & !is.na(harv_tmp[,paste0(val,"_location_3")])),
      #          "col_sel"] <- harv_tmp[which(harv_tmp$prov_location_3 == p & !is.na(harv_tmp[,paste0(val,"_location_3")])),
      #                                 paste0(val,"_location_3")] + harv_tmp[which(harv_tmp$prov_location_3 == p & !is.na(harv_tmp[,paste0(val,"_location_3")])),
      #                                                                       "col_sel"]
      #

      tmp_tech_sum <- data.frame(tech = techs)
      for(tech in techs){

      if(val1 == "active"){
        ty = "prop"
        val <- paste0("days_",tech)
      }else{
        ty = "number"
        val <- paste0(val1,"_",tech)
      }


      harv_tmp[,"col_sel"] <- 0

      harv_tmp[which(harv_tmp$prov_location_1 == p & !is.na(harv_tmp[,paste0(val,"_location_1")])),
               "col_sel"] <- harv_tmp[which(harv_tmp$prov_location_1 == p & !is.na(harv_tmp[,paste0(val,"_location_1")])),
                                      paste0(val,"_location_1")] + harv_tmp[which(harv_tmp$prov_location_1 == p & !is.na(harv_tmp[,paste0(val,"_location_1")])),
                                                                            "col_sel"]
      harv_tmp[which(harv_tmp$prov_location_2 == p & !is.na(harv_tmp[,paste0(val,"_location_2")])),
               "col_sel"] <- harv_tmp[which(harv_tmp$prov_location_2 == p & !is.na(harv_tmp[,paste0(val,"_location_2")])),
                                      paste0(val,"_location_2")] + harv_tmp[which(harv_tmp$prov_location_2 == p & !is.na(harv_tmp[,paste0(val,"_location_2")])),
                                                                            "col_sel"]
      harv_tmp[which(harv_tmp$prov_location_3 == p & !is.na(harv_tmp[,paste0(val,"_location_3")])),
               "col_sel"] <- harv_tmp[which(harv_tmp$prov_location_3 == p & !is.na(harv_tmp[,paste0(val,"_location_3")])),
                                      paste0(val,"_location_3")] + harv_tmp[which(harv_tmp$prov_location_3 == p & !is.na(harv_tmp[,paste0(val,"_location_3")])),
                                                                            "col_sel"]

  if(ty == "prop"){

        harv_tmp <- harv_tmp %>%
        mutate(col_sel = ifelse(col_sel > 0,
                                1,
                                NA))

  }else{
    harv_tmp <- harv_tmp %>%
      mutate(col_sel = ifelse(col_sel > 0,
                              col_sel,
                              NA))
}



      tmp_tech_sum[which(tmp_tech_sum$tech == tech),"sum1"] <- sum(harv_tmp$col_sel,na.rm = TRUE)

      }


      tmp_pop <- round(unname(unlist(pop_tmp1[,val1])))
      tmp_tech_sum <- tmp_tech_sum %>%
        mutate(proportion = sum1/sum(sum1),
               estimate = round(proportion*tmp_pop),
               #mean_hunter = sum1/nrow(harv_tmp),
               stratum = st,
               prov = p,
               value = val1)


      tech_sum_out <- bind_rows(tech_sum_out,
                                tmp_tech_sum)


    }#v
    }#i


#
#   sum_tmp <- out_sel_d %>%
#     as.data.frame()
#
#   out_sel_d <- out_sel_d %>%
#     mutate(province = p)
#   if(j == 1){
#     out_sel_d_all <- out_sel_d
#   }else{
#     out_sel_d_all <- bind_rows(out_sel_d_all,out_sel_d)
#   }
#
#
#   for(v in 1:nrow(vals_t)){
#
#     hdr <- vals_t[v,"hdr"]
#     m_y <- sum(as.numeric(sum_tmp[,paste0(hdr,"_est")]),na.rm = TRUE)
#     se_y <- sqrt(sum(as.numeric(sum_tmp[,paste0(hdr,"_se")])^2,na.rm = TRUE))
#
#     out_dd[j,hdr] <- paste(round(m_y),"\U00B1",round(se_y))
#
#
#
#   }

}


dist_link <- out_d %>%
  select(prov,n_active,days,tot_harv) %>%
  pivot_longer(cols = c(n_active,days,tot_harv),
               values_to = "est_total") %>%
  mutate(value = ifelse(name == "n_active",
                        "active",name),
         value = ifelse(name == "tot_harv",
                        "killed",value)) %>%
  select(-name)

sum_tech_dist <- tech_sum_out %>%
  left_join(dist_link, by = c("prov","value")) %>%
  mutate(p_tot1 = estimate/est_total) %>%
  group_by(tech,prov,value) %>%
  summarise(est = sum(estimate,na.rm = TRUE),
            p_tot = round(sum(p_tot1,na.rm = TRUE),2)) %>%
  arrange(value,prov) %>%
  pivot_wider(names_from = c(value,tech),
              values_from = c(est,p_tot))




saveRDS(sum_tech_dist,"Western/output/tech_harvest_west.rds")






# Calendar summary and plotting -------------------------------------------


bags <- readxl::read_xlsx("Western/protected/Ques4.xlsx",
                         col_types = c(rep("numeric",4))) %>%
  rename_with(.,.fn = str_to_lower) %>%
  mutate(date = ymd(paste(y,month,day,sep = "/")),
         killed = ifelse(killed < 0,NA,killed),
         killed = ifelse(killed > 50,50,killed)) %>%
  filter(!is.na(date),
         !is.na(killed)) %>%
  #select(id,killed,date) %>%
  arrange(date) %>%
  left_join(ids_strats,by = "id") %>%
  filter(!is.na(stratum))


##  calculate the mean kill per day for hunters in each stratum
##  calculate the ratio of the total harvest to the mean kill per day (days)
##  multiply that ratio by the proportion of the hunters reporting each bag



harv_summary <- out_sel %>%
  select(stratum,season,`Number of Active Hunters_est`,`Number of Days_est`,`Harvest of All Geese_est`) %>%
  rename(n_active = `Number of Active Hunters_est`,
         n_days = `Number of Days_est`,
         harv = `Harvest of All Geese_est`)


kpd_by_strat <- bags %>%
  group_by(stratum) %>%
  summarise(kpd = mean(killed),
            tot_count = n()) %>%
  inner_join(harv_summary,
             by = "stratum") %>%
  mutate(factor = harv/kpd)

rep_bags_strat <- bags %>%
  group_by(stratum,killed) %>%
  summarise(count = n()) %>%
  left_join(kpd_by_strat,by = "stratum") %>%
  mutate(prop = count/tot_count,
         estdb = factor*prop) %>%
  select(stratum,season,killed,count,kpd,
         tot_count,harv,factor,prop,estdb) %>%
  arrange(killed)

tabl9 <- rep_bags_strat %>%
  select(killed,stratum,season,estdb) %>%
  arrange(stratum,killed) %>%
  rename(daily_bag = killed) %>%
  pivot_wider(id_cols = daily_bag,
              names_from = c(stratum,season),
              values_from = estdb,
              values_fill = 0) %>%
  arrange(daily_bag) %>%
  rowwise() %>%
  mutate(across(AB_regular:ZU_spring,.fn = round),
         total = sum(c_across(AB_regular:ZU_spring),na.rm = TRUE),
         harvest = total*daily_bag)

print(paste("Total goose harvest is",round(sum(harv_summary$harv)),
            "Total harvest by daily bag is",sum(tabl9$harvest)))
saveRDS(tabl9, "Western/output/tb9.rds")




# Seasonal pattern in harvest and activity --------------------------------



calendars <- readxl::read_xlsx("Western/protected/Ques4.xlsx",
                              col_types = c(rep("numeric",4))) %>%
  rename_with(.,.fn = str_to_lower) %>%
  mutate(date = ymd(paste(y,month,day,sep = "/")),
         killed = ifelse(killed > 50,50,killed)) %>%
  filter(!is.na(date),
         !is.na(killed)) %>%
  select(id,killed,date) %>%
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

saveRDS(seasonal_pattern,"Western/output/seasonal_patterns.rds")



# table 10 ----------------------------------------------------------------

add_row_table10 <- FALSE # set this to TRUE for the first run then return to FALSE to avoid overwriting (or appending multiple values for this year)
if(add_row_table10){
#tab10 <- readxl::read_xlsx("Western/table10_historical.xlsx")
tab10 <- read_csv(paste0("Western/table10_historical.csv"))
strat_join2 <- data.frame(stratum = c(strat_join$stratum,"Total")) %>%
  mutate(season1 = c("spring","spring","spring",
                    "regular season","regular season","regular season",
                    NA),
         prov = c("AB","SK","US","AB","SK","US","Total"))

tab10_sub <- out %>%
  filter(!(stratum == "Total" & !is.na(season))) %>%
  inner_join(strat_join2) %>%
  ungroup() %>%
  mutate(hdr = paste(prov,season1),
         hdr = gsub(" NA","",hdr),
         Year = y) %>%
  select(`Harvest of All Geese`,hdr,Year) %>%
  pivot_wider(names_from = hdr,
              id_cols = Year,
              values_from = `Harvest of All Geese`)

tab10 <- tab10 %>%
  bind_rows(tab10_sub)

saveRDS(tab10,"Western/output/table10.rds")
write_excel_csv(tab10,paste0("Western/table10_historical.csv"))

}
