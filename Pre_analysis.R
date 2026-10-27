
# # To be run during Analysis -----------------------------------------------
#
#This first identifies the list of permits that were selected
#to match them with the permits associated with the responses
#
#Then it calculates the total populations of hunters for spring harvest
#in the following 3 steps
#
# 1, it generates a spatial map of the eastern hunting strata
#based on the spring sampling zones in quebec and ontario
#
# 2 it creates a latitude and longitude of spring hunting and
#selects out the hunting permits that are relevant to this regional
#harvest.
#
# 3, with a spatial join, it allocates
#the regular season and spring permits to the spring hunting strata
#

Y <- 2025 ### modify to current calendar year
Yl <- Y-1

diry <- "Eastern" #paste("m:/My Documents/Snow Geese/A229/Quebec",Y,sep = "")
#setwd(paste(diry,"/ANALYSIS",sep = ""))
library(sp)
#library(maptools)
#library(rgeos)
#library(foreign)
library(tidyverse)
library(sf)

zs <- c("A","B","C","D","E","F","G","H","I","J")

provs <- c("ON","PQ")


# list out the permits that were selected for the analysis ----------------

finalperms <- readxl::read_xlsx(paste("Data/Protected/Permits 2024 (as of September 5 2025).xlsx"),
                                guess_max = min(5000, Inf))

names(finalperms)[1] <- "permit_type"
names(finalperms) <- tolower(names(finalperms))



oselq <- read.table(paste("Eastern/SSGS",Y,"PQ.txt"),stringsAsFactors = F)
names(oselq) <- "permit"
oselo <- read.table(paste("Eastern/SSGS",Y,"ON.txt"),stringsAsFactors = F)
names(oselo) <- "permit"

osel <- rbind(oselq,oselo)
############################ identify the permits selected in the spring

psel <- finalperms %>%
  as.data.frame() %>%
  filter(grepl(permit_type,pattern = "pring"),
         grepl(permit_type,pattern = "astern"),
         !grepl(permit_type,pattern = "not selected")) %>%
  select(permit)


# #################### stack osel and psel
asel <- osel %>%
  bind_rows(.,psel)


missing <- asel[!(asel$permit %in% finalperms$permit),]

if(length(missing) > 0){
warning(paste("There are",
        length(missing),
        "selected permits that are not in the
        final permit file.
        Next line will remove these permits from the
        list of selected permits"))
}
asel <- asel %>%
  filter(!permit %in% missing)

# asel object just needs the information on season and
# stratum - which are added after the permits are
# allocated to strata below.
#



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
plot(wmu)


all_eastern_map <- bind_rows(QC_zones,wmu) %>%
  ungroup() %>%
  mutate(zone = as.character(zone)) %>%
  group_by(zone) %>%
  summarise()
plot(all_eastern_map)




# Identify the spring hunting zones ---------------------------------------

# 2 it creates a latitude and longitude of spring hunting and
#selects out the hunting permits that are relevant to this regional
#harvest.


## The estimates here apply to the population of hunters who:
## 1) purchased a migratory game bird hunting permit in the spring
## (hereafter spring permits) in ON or QC, or online and either
## live in one of the regions or indicated they would hunt in these
## regions - identified by Michel in the permit file; and -
## 2) Ontario and Quebec residents
## who purchased a permit during the regular hunting season, and live
## where the spring hunt takes place (in Quebec, hunting
## districts D, E, F, G, H, or I, in Ontario, Wildlife Management
## Unit 65 (WMU) 65) or in the Ontario WMUs that surround WMU 65
## (hereafter regular season permits).
##


spring_keep <- which(grepl(finalperms$permit_type,pattern = "Spring") &
                       grepl(finalperms$permit_type, pattern = "astern"))


regular_prov_residents <- which((grepl(finalperms$permit_type,pattern = "Regular") &
                       finalperms$province %in% provs) |
                         finalperms$permit %in% asel$permit)


finalperms_o <- finalperms

finalperms <- finalperms[unique(c(spring_keep,regular_prov_residents)),]

chk <- asel %>% filter(!permit %in% finalperms$permit)

if(nrow(chk) > 0){
  warning("permits are missing")
}

#replacing missing lat-long info
finalperms[which(grepl("Spring",finalperms$permit_type)),"salelat"] <- finalperms[which(grepl("Spring",finalperms$permit_type)),"hunting_latitude"]
finalperms[which(grepl("Spring",finalperms$permit_type)),"salelon"] <- finalperms[which(grepl("Spring",finalperms$permit_type)),"hunting_longitude"]


finalperms[which(grepl("Regular",finalperms$permit_type)),"salelat"] <- finalperms[which(grepl("Regular",finalperms$permit_type)),"residence_latitude"]
finalperms[which(grepl("Regular",finalperms$permit_type)),"salelon"] <- finalperms[which(grepl("Regular",finalperms$permit_type)),"residence_longitude"]



finalperms[which(is.na(finalperms[,"salelat"])),"salelat"] <- finalperms[which(is.na(finalperms[,"salelat"])),"residence_latitude"]
finalperms[which(is.na(finalperms[,"salelon"])),"salelon"] <- finalperms[which(is.na(finalperms[,"salelon"])),"residence_longitude"]

finalperms[which(is.na(finalperms[,"salelat"])),"salelat"] <- finalperms[which(is.na(finalperms[,"salelat"])),"hunting_latitude"]
finalperms[which(is.na(finalperms[,"salelon"])),"salelon"] <- finalperms[which(is.na(finalperms[,"salelon"])),"hunting_latitude"]



finalperms <- finalperms %>%
  filter(!is.na(salelat),
         !is.na(salelon)) %>%
  mutate(latd = floor(salelat/100),
         latm = (salelat/100 - latd)*100,
         latdd = latd + latm/60,
         longd = floor(salelon/100),
         longm = (salelon/100 - longd)*100,
         longdd = -1*(longd + longm/60))

tst <- finalperms %>%
  filter(is.na(latdd))

if(nrow(tst) > 0){
  stop("There are some permits without coordinates")
}

permit_map <- finalperms
coordinates(permit_map) <- c("longdd","latdd")

permit_map <- st_as_sf(permit_map)
st_crs(permit_map) <- 4269 #NAD83 - assumption that this is probably what these coordinates represent

laea = st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area

permit_map <- st_transform(permit_map, laea)
all_eastern_map <- st_transform(all_eastern_map,laea)
#all_eastern_map <- st_buffer(all_eastern_map,dist = 10000)

permit_zone1 <- st_join(permit_map,all_eastern_map,
                       join = st_intersects,
                       largest = TRUE)# largest means only the fields of y with largest overlap - using largest = TRUE ensures that no rows are replicated if their points intersect more than one polygon



# Drop all permits outside of sampling regions ----------------------------
# except spring sampled permits

permit_zone <- permit_zone1 %>%
  filter((!is.na(zone) |
            grepl("Spring",permit_type) |
            permit %in% asel$permit)) %>%
  mutate(zone = ifelse(is.na(zone) &
                         province == "ON",
                       "P",zone),
         zone = ifelse(is.na(zone) &
                         province == "PQ",
                       "H",zone))

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

# permits_spring1 <- permit_zone1 %>%
#   filter(is.na(zone)) %>%
#   select(-zone)
#
# east_buf <- st_buffer(all_eastern_map,
#                       dist = 1000)
# permits_spring <- st_join(permits_spring1,
#                           east_buf,
#         join = st_intersects,
#         largest = TRUE) %>%
#   filter(!is.na(zone))
#
#
# lims <- st_bbox(all_eastern_map)
#
# mp1 <- ggplot(data = all_eastern_map,
#               aes(colour = zone))+
#   #geom_sf(data = permit_zone1,alpha = 0.2,
#   #        aes(colour = zone))+
#   geom_sf(data = permits_spring)+
#   geom_sf(alpha = 0.05)
#
# print(mp1)



# tst <- permit_zone %>% filter(is.na(zone),
#                               !grepl("Spring",permit_type))
#
# mp1 <- ggplot(data = all_eastern_map,
#               aes(colour = zone))+
#   geom_sf(data = tst,alpha = 0.2,
#           aes(colour = zone))+
#   #geom_sf(data = permits_spring)+
#   geom_sf(alpha = 0.05)+
#   coord_sf(xlim = lims[c("xmin","xmax")],
#            ylim = lims[c("ymin","ymax")])
#
# print(mp1)


# mp1 <- ggplot(data = all_eastern_map)+
#   geom_sf(data = permit_zone,alpha = 0.2,
#           aes(colour = zone))+
#   geom_sf(alpha = 0.05)
#
# print(mp1)


allr <- permit_zone %>%
  st_drop_geometry() %>%
  filter(permit_type %in% c("Regular","unknown"))

alls <- permit_zone %>%
  st_drop_geometry() %>%
  filter(grepl(permit_type, pattern = "Spring",fixed = T))


#spring permits by hunting district
 table(alls$zone, useNA = "always")

#A   B   C   D   E   F   G   H   I
#10  20  29  24  83 679 145 819  56   4
 #total number of spring permits
 sum(table(alls$zone, useNA = "always"))
# [1] 1869


allpermits <- permit_zone %>%
  select(permit,permit_type,
         zone)



springpops <- alls %>%
  as.data.frame() %>%
  ungroup() %>%
  group_by(zone) %>%
  summarise(n_permits = n()) %>%
  select(zone,n_permits)


# Export the population sizes ---------------------------------------------


write.csv(springpops,"Eastern/final spring populations4.csv",row.names = F)


regpops <- allr %>%
  as.data.frame() %>%
  ungroup() %>%
  group_by(zone) %>%
  summarise(n_permits = n()) %>%
  select(zone,n_permits)

write.csv(regpops,"Eastern/final regular populations4.csv",row.names = F)






# export the selected permits for SAS program -----------------------------
asel[!(asel$permit %in% permit_zone$permit),]


asel1 <- asel %>%
  left_join(.,permit_zone,by = "permit") %>%
  mutate(season = tolower(permit_type),
         season = ifelse(grepl(season,pattern = "spring"),"spring",season))

# tst <- asel1 %>% filter(is.na(zone))
#
# tst2 <- finalperms_o %>%
#   filter(permit %in% tst$permit)
#
# tst3 <- tst2 %>%
#   filter(!is.na(salelat),
#          !is.na(salelon)) %>%
#   mutate(latd = floor(salelat/100),
#          latm = (salelat/100 - latd)*100,
#          latdd = latd + latm/60,
#          longd = floor(salelon/100),
#          longm = (salelon/100 - longd)*100,
#          longdd = -1*(longd + longm/60))
#
# coordinates(tst3) <- c("longdd","latdd")
#
# tst3 <- st_as_sf(tst3)
# st_crs(tst3) <- 4269 #NAD83 - assumption that this is probably what these coordinates represent
#
#
# tst3 <- st_transform(tst3, laea)
#
# mp1 <- ggplot(data = all_eastern_map)+
#   geom_sf(data = tst3,alpha = 0.2,
#           aes(colour = saleprov))+
#   #geom_sf(data = permits_spring)+
#   geom_sf(alpha = 0.05)
#
# print(mp1)
#

if(nrow(osel)+nrow(psel) != (nrow(asel)+length(missing))){
  stop("Selected permits don't match permit file")
}

#export the list of selected permits with their zones
regsel <- asel1 %>%
  filter(!is.na(zone)) %>%
  select(permit,zone,season) %>%
  mutate(id = Yl*1000000 + permit,
         stratum = as.character(zone))

if(length(which(is.na(regsel$stratum))) > 0){

  write.csv(regsel[-which(is.na(regsel$stratum)),],"Eastern/selwspr.csv",row.names = F)
}else{
  write.csv(regsel,"Eastern/selwspr.csv",row.names = F)

}


