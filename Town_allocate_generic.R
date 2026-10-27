#### Allocating towns to hunting districts Spring Snow Goose Survey
library(tidyverse)
library(sf)
library(stringdist)
library(fuzzyjoin)
y = 2024

ly = y-1

prov_sel <- c("ON","QC")




QC_zones <- readRDS("GIS/Quebec_historical_hunting_zones.rds") %>%
  st_transform(.,crs = 3348) #%>%  #Canada Lambert Conformal Conic (equidistance)


wmu.samp <- c("65","64A","64B","66A","67","68B","69A-1","69A-2","69A-3","69B","70","71")   ### vector of the Snow Goose wmus, in the correct order for the tables in the report

### zone P == WMU 65
### zone Q == outside WMU 65 in Ontario
###
wmu <- readRDS("GIS/Ontario_wmu_spring_Snow_goose_zones.rds") %>%
  mutate(zone = hunt_zone) %>%
  select(zone) %>%
  group_by(zone) %>%
  summarise()
plot(wmu)


all_eastern_map <- bind_rows(QC_zones,wmu)
plot(all_eastern_map)


can_town_names <- read_sf("GIS/cgn_canada_shp_eng.shp")


reg_town_nams <- can_town_names %>%
  filter(PROV_TERR != "Undersea Feature") %>%
  st_transform(crs = st_crs(all_eastern_map)) %>%
  st_join(.,all_eastern_map,
          join = st_intersects) %>%
  select(GEONAME,zone) %>%
  filter(!is.na(zone)) %>%
  mutate(town = str_to_lower(GEONAME),
         town = str_replace_all(town,"[[:punct:]]"," ")) %>%
  select(-GEONAME) %>%
  st_drop_geometry() %>%
  distinct()


# hunting_locations_region <- technique %>%
#   filter(id %in% full_sampled$id) %>%
#   group_by(locality,prov) %>%
#   summarise(number_hunters = n()) %>%
#   arrange(prov,-number_hunters)
# saveRDS(hunting_locations_region,"reported_hunting_locations.rds")




# location information for fixing missing towns ---------------------------
townnames <- read_fwf("Eastern/arch/analysis/townhunt2.txt",
                      fwf_widths(widths = c(41,1),
                                 col_names = c("TOWN_orig","district"))) %>%
  rowwise() %>%
  mutate(TOWN = str_extract(pattern = ".{1,}(?=,)",
                            TOWN_orig),
         TOWN = ifelse(is.na(TOWN),TOWN_orig,TOWN),
         town = str_to_lower(TOWN) )

miss_towns <- townnames %>%
  mutate(town = str_replace_all(str_to_lower(TOWN),"[[:punct:]]"," ") ,
         town = str_replace(town,"^st ","saint "),
         town = str_replace(town,"^sainte ","saint "),
         town = str_replace(town,"^ste ","saint ")) %>%
  filter(is.na(district))

## fuzzy matching with stringdist()
##

matched_data <- stringdist_join(
  miss_towns, reg_town_nams,
  by = "town",  # Column to match on
  mode = "left",  # Type of join: left, inner, or full
  method = "jw",  # Jaro-Winkler distance metric
  max_dist = 0.18,  # Maximum allowable distance for a match
  distance_col = "distance"
  ) %>%
  arrange(TOWN_orig,distance)

write_csv(matched_data,
          "temp_matched_towns.csv")
### after this, try fixing townhunt3.txt by hand
###

