
# requires: boundary files BOUND_.txt ### stored in working directory
#           PermitYYYY .csv #permits as of the time of sampling

Y <- 2026 ### modify to current calendar year
Yl <- Y-1

# diry <- paste("m:/My Documents/Snow Geese/A229/Quebec",Y,sep = "")
# setwd(paste(diry,"/DESIGN",sep = ""))
library(sf)
library(tidyverse)
library(readxl)

wd <- getwd()
ly_dir <- str_replace(getwd(),pattern = "/[[:digit:]]{4}",
                      replacement = paste0("/",Yl))


# function to calculate the sample size -----------------------------------
# checks to ensure all regional sample sizes > 100
# and that total samples == N

prop <- function(x,n){
  y <- vector("integer",length(x))
  for(i in 1:length(x)){
    y[i] <- round((x[i]/sum(x))*n)
  }
  # recalculate if any samples smaller than 100
  if(any(y < 100)){
    w_low <- which(y < 100)
    w_not_low <- which(y >= 100)
    y[w_low] <- 100
    n_adj <- n-(100*length(w_low))
    for(i in w_not_low){
      y[i] <- round((x[i]/sum(x[w_not_low]))*n_adj)
    }
  }
  # check sum of samples == N
  if(sum(y) != n){
    y[which.max(y)] <- y[which.max(y)]-(sum(y)-n)
  }
  return(y)
}




# All permits -------------------------------------------------------------


allp <- read_xlsx(path = paste0("data/protected/Permits ",Yl," (as of March 23 ",Yl+1,").xlsx"),
                  col_types = c("text",
                                "numeric",
                                "numeric",
                                "text",
                                "numeric",
                                "numeric",
                                "text",
                                "text",
                                "text",
                                "text",
                                "text",
                                "text",
                                "text",
                                "text",
                                "text",
                                "text",
                                "text",
                                "numeric",
                                "text",
                                "text",
                                "numeric",
                                "numeric",
                                "numeric",
                                rep("text",22),
                                rep("numeric",6),
                                "text",
                                "numeric",
                                rep("text",2))) %>%  #all of the permits in teh database at the time of sampling
filter(mailcode == "Y")

names(allp)[1] <- "permit_type"


springperm <- allp %>%
  filter(grepl("pring",permit_type))

regularpermits <- allp %>%
  filter(!grepl("pring",permit_type))

##### select and generate the final location coordinate:


## residence location is primary location
## this makes sense because most of the hunting locations
## reflect fall hunting locations, and we are hoping to sample spring hunting location

regularpermits[,"latitude"] <- regularpermits[,"residence_latitude"]
regularpermits[,"longitude"] <- regularpermits[,"residence_longitude"]


## sale location is secondary location

salesamp <- which((regularpermits$saleprov == regularpermits$sampling_prov &
                     regularpermits$salezone == regularpermits$sampling_zone)
                  & is.na(regularpermits$latitude))

regularpermits[salesamp,"latitude"] <- regularpermits[salesamp,"salelat"]
regularpermits[salesamp,"longitude"] <- regularpermits[salesamp,"salelon"]


## hunting location is tertiary location

huntsamp <- which((regularpermits$hunting_province == regularpermits$sampling_prov &
                    regularpermits$hunting_zone == regularpermits$sampling_zone)
                  & is.na(regularpermits$latitude))

regularpermits[huntsamp,"latitude"] <- regularpermits[huntsamp,"hunting_latitude"]
regularpermits[huntsamp,"longitude"] <- regularpermits[huntsamp,"hunting_longitude"]

no_location_test <- regularpermits %>%
  filter(is.na(latitude) | is.na(longitude),
         (!is.na(province))) #drops non-canadian permits (no province info)

  if(nrow(no_location_test) > 0){
    warning("There are ",nrow(no_location_test)," canadian permits with no location information")
write.csv(no_location_test,"testregularpermits.csv")
view(no_location_test)
### check above csv file
### it should only include:
  ### Canadian hunters with no location information or a different sampling
    ## zone from their residence zone

  }

regularpermits_west <- regularpermits %>%
  filter((sampling_prov %in% c("AB","SK") & !is.na(state)) |
           province %in% c("AB","SK","YT","NU","NT"),
         is.na(country),
         (!is.na(province) | !is.na(state))) %>%
  mutate(latd = as.integer(floor(latitude/100)),
         latm = as.integer(((latitude/100)-latd)*100),
         latdd = latd + latm/60,
         longd = as.integer(floor(longitude/100)),
         longm = as.integer(((longitude/100)-longd)*100),
         longdd = -1*(longd + longm/60),
         sample_zone = ifelse(!is.na(province),
                              province,"US"))

### remove the NA longitude and latitude rows?
regularpermits_east = regularpermits %>%
  filter(!is.na(latitude) & !is.na(longitude),
         is.na(country),
         is.na(state)) %>%
  mutate(latd = as.integer(floor(latitude/100)),
         latm = as.integer(((latitude/100)-latd)*100),
         latdd = latd + latm/60,
         longd = as.integer(floor(longitude/100)),
         longm = as.integer(((longitude/100)-longd)*100),
         longdd = -1*(longd + longm/60))





# Regional ----------------------------------------------------------

region = "Eastern"

wmu.samp <- c("65","64A","64B","66A","67","68B","69A-1","69A-2","69A-3","69B","70","71")   ### vector of the Snow Goose wmus, in the correct order for the tables in the report

# Spatial data for Eastern zones ------------------------------------------
QC_zones <- readRDS("GIS/Quebec_historical_hunting_zones.rds") %>%
  st_transform(.,crs = 3348) %>%  #Canada Lambert Conformal Conic (equidistance)
  mutate(sample_zone = zone) %>%
  select(sample_zone)
# 4326 - common WGS unprojected
# wmu <- read_sf(dsn = "GIS/arch",
#                layer = "WMU_june1_2006") %>%
#   st_transform(.,st_crs(QC_zones)) %>%
#   mutate(wmu = OFFICIAL_N,
#          sample_zone = ifelse(wmu %in% wmu.samp,"inside","outside"),
#          hunt_zone = ifelse(wmu %in% wmu.samp,"P","Q"),
#          hunt_zone = ifelse(wmu == "65","O",hunt_zone)) %>%
#   select(wmu,sample_zone,hunt_zone)
# saveRDS(wmu,"GIS/Ontario_wmu_spring_Snow_goose_zones.rds")

wmu <- readRDS("GIS/Ontario_wmu_spring_Snow_goose_zones.rds") %>%
  filter(spring_hunt == "inside")



for(prov_sel in c("PQ","ON")){

if(prov_sel == "PQ"){
    regs_map <- QC_zones
  }else{
    regs_map <- wmu
  }

reg_sel <- regularpermits_east %>%
  filter(province == prov_sel,
         sampling_prov == prov_sel)


sel_loc <- reg_sel %>%
  mutate(xcoord = longdd,
         ycoord = latdd) %>%
  #select(longdd,latdd,permit) %>%
  st_as_sf(.,
           coords = c("xcoord","ycoord")) %>%
  st_set_crs(.,4326) %>% #WGS lat long
  st_transform(.,st_crs(regs_map))

#
# tst <- ggplot()+
#   geom_sf(data = regs_map)+
#   geom_sf(data = sel_loc)
# tst

if(prov_sel == "QC"){
maps_sel <- sel_loc %>%
  st_join(.,regs_map)
}else{
  maps_sel <- sel_loc %>%
    st_join(.,regs_map,
            left = FALSE) # drops permits outside of relevant zones
}
near <- maps_sel %>%
  filter(is.na(sample_zone))

if(nrow(near) > 0){
dist_sel_near <- near %>%
  select(-sample_zone) %>%
  nngeo::st_nn(.,regs_map)



for(j in 1:nrow(near)){
  p <- near$permit[j]
  wz <- dist_sel_near[[j]]
  z <- regs_map$sample_zone[wz]

  wp <- which(maps_sel$permit == p)
  if(length(wp) > 1){stop("repeated permit values ",p)}
  maps_sel[wp,"sample_zone"] <- z
}

}

# tst <- ggplot()+
#   geom_sf(data = regs_map)+
#   geom_sf(data = maps_sel,
#           aes(colour = sample_zone))
# tst


ttt <- maps_sel %>%
  st_drop_geometry() %>%
  summarise(.,
            .by = sample_zone,
            n_permits = n())




# import saved data on harvest sd -----------------------------------------

alloc <- read_csv(paste0(ly_dir,"/Eastern/",prov_sel," final allocations.csv"),
                  show_col_types = FALSE) %>%
  select(sample_zone,sdharv) %>%
  rename(sd_lasty = sdharv)


sd_ly <- readRDS(paste0(ly_dir,"/Eastern/harv.rds")) %>%
  ungroup() %>%
  group_by(analysis_stratum) %>%
  summarise(n = n(),
            mean_kill = mean(kill,na.rm = TRUE),
            sd_kill = sd(kill,na.rm = TRUE))

if(prov_sel == "ON"){
  sd_ly <- sd_ly %>%
    mutate(analysis_stratum = ifelse(analysis_stratum == "O",
                                     "inside",analysis_stratum),
           analysis_stratum = ifelse(analysis_stratum == "P",
                                     "outside",analysis_stratum))
}

alloc <- alloc %>%
  left_join(sd_ly, by = c("sample_zone" = "analysis_stratum")) %>%
  mutate(sdharv = (sd_lasty*(Y-2020)+sd_kill)/((Y-2020)+1)) %>%  # weighted mean sd based on the number of years of previous data included in last year's estimate
  select(sample_zone,sdharv)

N <- ifelse(prov_sel == "PQ",3200,1600) # number of permits to sample


# Drops all but relevant zones --------------------------------------------
allocation <- ttt %>%
  inner_join(.,alloc,
             by = "sample_zone") %>%
  mutate(sample_size = prop(sdharv*n_permits,N))



write_csv(allocation,paste0("Eastern/",prov_sel," final allocations.csv"))

set.seed(Y)
perm_sel <- maps_sel %>%
  st_drop_geometry() %>%
  group_by(sample_zone) %>%
  nest() %>%
  ungroup() %>%
  inner_join(.,allocation,by = "sample_zone") %>%
  mutate(samp = map2(data, sample_size, sample_n)) %>%
  select(-data) %>%
  unnest(samp) %>%
  select(permit)


perms_all <- maps_sel %>%
  st_drop_geometry() %>%
  mutate(selected = ifelse(permit %in% perm_sel$permit,
                           TRUE,FALSE))

tst <- perms_all %>%
  group_by(sample_zone,selected) %>%
  summarise(n_perms = n())

write_csv(tst,paste0("Eastern/",prov_sel,"realised_sampling_alloc.csv"))

tst <- tst %>%
  filter(selected)
if(sum(tst$n_perms) != N){
  stop("There are permits missing from the selection")
}

if(nrow(perm_sel) != N){
  stop("There are permits missing from the selection")
}

saveRDS(perms_all,paste0("Eastern/protected/",prov_sel," permits with zones and selection.rds"))
write.table(perm_sel,paste0("Eastern/SSGS ",Y," ",prov_sel,".TXT",sep = ""),row.names = F,quote = F,col.names = F)


}# end of prov loop





# Western -----------------------------------------------------------------
if(any(file.exists(paste0("Western/SSGS ",Y," ",c("SK","AB"),".TXT",sep = "")))){
  stop("At least one of the sampling files exists.")
}

for(prov_sel in c("SK","AB")){
  reg_sel <- regularpermits_west %>%
    filter(sample_zone == prov_sel |
           (sampling_prov == prov_sel & !is.na(state)))


  ttt <- reg_sel %>%
    summarise(.,
              .by = sample_zone,
              n_permits = n())


  alloc <- read_csv(paste0(ly_dir,"/Western/",prov_sel," final allocations.csv"),
                    show_col_types = FALSE) %>%
    select(sample_zone,sdharv) %>%
    rename(sd_lasty = sdharv)


  sd_ly <- readRDS(paste0(ly_dir,"/Western/output/harv.rds")) %>%
    ungroup() %>%
    group_by(stratum) %>%
    summarise(n = n(),
              mean_kill = mean(total,na.rm = TRUE),
              sd_kill = sd(total,na.rm = TRUE))

  # if(prov_sel == "ON"){
  #   sd_ly <- sd_ly %>%
  #     mutate(analysis_stratum = ifelse(analysis_stratum == "O",
  #                                      "inside",analysis_stratum),
  #            analysis_stratum = ifelse(analysis_stratum == "P",
  #                                      "outside",analysis_stratum))
  # }

  alloc <- alloc %>%
    left_join(sd_ly, by = c("sample_zone" = "stratum")) %>%
    mutate(sdharv = (sd_lasty*(Y-2020)+sd_kill)/((Y-2020)+1)) %>%  # weighted mean sd based on the number of years of previous data included in last year's estimate
    select(sample_zone,sdharv)

  N <- ifelse(prov_sel == "AB",2700,2150)

  # Drops all but relevant zones --------------------------------------------
  allocation <- ttt %>%
    inner_join(.,alloc,
               by = "sample_zone") %>%
    mutate(sample_size = prop(sdharv*n_permits,N))


  write_csv(allocation,paste0("Western/",prov_sel," final allocations.csv"))

  set.seed(Y)
  perm_sel <- reg_sel %>%
    group_by(sample_zone) %>%
    nest() %>%
    ungroup() %>%
    inner_join(.,allocation,by = c("sample_zone")) %>%
    mutate(samp = map2(data, sample_size, sample_n)) %>%
    select(-data) %>%
    unnest(samp) %>%
    select(permit)

  if(nrow(perm_sel) != N){
    stop("There are permits missing from the selection")
  }

  perms_all <- reg_sel %>%
    mutate(selected = ifelse(permit %in% perm_sel$permit,
                             TRUE,FALSE))

  tst <- perms_all %>%
    group_by(sample_zone,selected) %>%
    summarise(n_perms = n())

  write_csv(tst,paste0("Western/",prov_sel,"realised_sampling_alloc.csv"))

  tst <- tst %>%
    filter(selected)
  if(sum(tst$n_perms) != N){
    stop("There are permits missing from the selection")
  }

  saveRDS(perms_all,paste0("Western/protected/",prov_sel," permits with zones and selection.rds"))
  write.table(perm_sel,paste0("Western/SSGS ",Y," ",prov_sel,".TXT",sep = ""),row.names = F,quote = F,col.names = F)




}




# Northern -----------------------------------------------------------------

prov_sel <- "YT_NT_NU"

reg_sel <- regularpermits_west %>%
  filter(sample_zone %in% c("YT","NT","NU")) %>%
  mutate(selected = TRUE)
perm_sel <- reg_sel %>%
  select(permit)

saveRDS(reg_sel,paste0("Northern/protected/",prov_sel," permits with zones and selection.rds"))
write.table(perm_sel,paste0("Northern/SSGS ",Y," ",prov_sel,".TXT",sep = ""),row.names = F,quote = F,col.names = F)






# Testing -----------------------------------------------------------------
regs <- data.frame(region = c("Eastern","Eastern","Western","Western","Northern"),
                   provs = c("PQ","ON","SK","AB","YT_NT_NU"))
perms_bind <- NULL
for(i in 1:nrow(regs)){
  region <- regs[i,"region"]
  prov_sel <- regs[i,"provs"]
  perms_all <- readRDS(paste0(region,"/protected/",prov_sel," permits with zones and selection.rds")) %>%
    mutate(region_sel = region,
           prov_selection = prov_sel)

  perms_bind <- bind_rows(perms_bind,perms_all)
}


perms_map <- perms_bind %>%
  filter(!is.na(latdd),
         selected) %>%
  mutate(reg_zone = paste(region_sel,prov_selection,sample_zone,sep = "-")) %>%
  st_as_sf(.,
           coords = c("longdd","latdd")) %>%
  st_set_crs(.,4326) %>% #WGS lat long
  st_transform(.,st_crs(regs_map))

perms_no_map <- perms_bind %>%
  filter(is.na(latdd))


test_base <- bbsBayes2::load_map("prov_state") %>%
  st_transform(.,st_crs(regs_map))

test_map <- ggplot()+
  geom_sf(data = test_base)+
  geom_sf(data = perms_map,
          aes(colour = reg_zone))


  test_map










