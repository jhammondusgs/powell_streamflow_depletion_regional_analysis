################################################################################### 
################################################################################### 
################################################################################### 
# Code to experiment with development of a classification scheme to separate gages likely impacted by pumping from those not impacted
# Goal:
#  Develop a classification of watersheds that gives an indication of whether or not a watershed is likely impacted by streamflow depletion. Ideally develop a classification that can tell us whether depletion likely constant through time, or whether depletion has likely increased substantially through time.
# To do:
#  Jessi - aggregating USDA crop data for multiple time steps to county level for use with the vulnerability map, and to watershed scale for use in classification of watersheds into likelihood of GW pumping and streamflow depletion impact
#  John - combining data from Jessi with most recent USGS GW and SW water use to get understanding on magnitude of water use and the ratio of GW to SW water use at each watershed. Also prepare transmissivity information from Andrea for use in watershed classification. Develop initial classification structure
################################################################################### 
################################################################################### 
###################################################################################
library(googledrive)
library(stringr)
library(maps)
library(viridis)
library(scales)
library(ggnewscale)
library(mapproj)
library(foreign)
library(raster)
library(exactextractr)
library(sf)
library(ggplot2)
# just for doing things old school without creating an R project. Change this to the location you'd like to work in.
base_wd <- "/Users/johnhammond/powell_streamflow_depletion_regional_analysis/"
setwd(base_wd)
dir.create(paste0(base_wd, "watershed_classification"))
setwd(paste0(base_wd, "watershed_classification"))
# access files from google drive
my_url <- "https://drive.google.com/drive/folders/1GS31PaawF0AGGuGnwQ3ThpFSkOd85-XZ"
x <- drive_ls(as_id(my_url))
y <- drive_ls(x$id[1]) # master files folder on google drive
for(i in 1:34){drive_download(y$id[i])} # download master files locally
###
states <- map_data("state")
counties <- read.dbf("county_areas.dbf")
counties$FIPS <- as.character(counties$FIPS)
gages2fields <- read.csv("common_metrics_of_interest_gages_2.csv")
gages2fields$FIPS_SITE <- str_pad(gages2fields$FIPS_SITE,5,"left","0")
# 2015 county level wayer use:
#Dieter, C.A., Linsey, K.S., Caldwell, R.R., Harris, M.A., Ivahnenko, T.I., Lovelace, J.K., Maupin, M.A., and Barber, N.L., 2018, Estimated Use of Water in the United States County-Level Data for 2015 (ver. 2.0, June 2018): U.S. Geological Survey data release, https://doi.org/10.5066/F7TB15V5.
wu2015 <- read.csv("usco2015v2.0.csv", header = TRUE)
wu2015$FIPS <- str_pad(wu2015$FIPS,5,"left","0")
gages2withWU <- merge(gages2fields, wu2015, by.x = "FIPS_SITE", by.y = "FIPS", all.x = TRUE)
  # <attrlabl>IR-WGWFr</attrlabl>
  # <attrdef>Irrigation, groundwater withdrawals, fresh, in Mgal/d</attrdef>
  # <attrlabl>IR-WSWFr</attrlabl>
  # <attrdef>Irrigation, surface-water withdrawals, fresh, in Mgal/d</attrdef>
  # <attrlabl>IR-WFrTo</attrlabl>
  # <attrdef>Irrigation, total withdrawals, fresh, in Mgal/d</attrdef>

# need to normalize by area of county!!!!
gages2withWU <- merge(gages2withWU, counties, by.x = "FIPS_SITE", by.y = "FIPS", all.x = TRUE)
# simple plotting of 2015 total irrigation water use information by gage
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = IR.WFrTo/area_sq_km)) +
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log")+coord_map("albers", lat0=30, lat1=40)
# simple plotting of 2015 GW sourced irrigation water use information by gage
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = IR.WGWFr/area_sq_km)) +
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log")+coord_map("albers", lat0=30, lat1=40)
# simple plotting of 2015 SW sourced irrigation water use information by gage
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = IR.WSWFr/area_sq_km)) +
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log")+coord_map("albers", lat0=30, lat1=40)
# gages2 watershed water use (1985-2010, 1 km grid to watersheds) and ag (sources of agricultural commodities such as crop types, irrigation, and livestock, 1950-2012)
# Falcone, J.A., 2017, U.S. Geological Survey GAGES-II time series data from consistent sources of land use, water use, agriculture, timber activities, dam removals, and other historical anthropogenic influences: U.S. Geological Survey data release, https://doi.org/10.5066/F7HQ3XS4.
landuse1974 <- read.csv("LandUse_NWALT_1974.txt")
landuse1982 <- read.csv("LandUse_NWALT_1982.txt")
landuse1992 <- read.csv("LandUse_NWALT_1992.txt")
landuse2002 <- read.csv("LandUse_NWALT_2002.txt")
landuse2012 <- read.csv("LandUse_NWALT_2012.txt")
landuse1974 <- landuse1974[,c(1:5)]
landuse1982 <- landuse1982[,c(1:5)]
landuse1992 <- landuse1992[,c(1:5)]
landuse2002 <- landuse2002[,c(1:5)]
landuse2012 <- landuse2012[,c(1:5)]
landuse <- cbind(landuse1974,landuse1982,landuse1992,landuse2002,landuse2012)
landuse <- landuse[,c(-6,-11,-16,-21)]
wateruse <- read.csv("WaterUse_1985-2010.txt")
agri <- read.csv("CensusOfAgriculture_1950-2012.txt")
# merge in these fields with gages2 data
gages2withWU <- merge(gages2withWU, landuse, by.x = "STAID", by.y = "STAID", all.x = TRUE)
gages2withWU <- merge(gages2withWU, wateruse, by.x = "STAID", by.y = "STAID", all.x = TRUE)
gages2withWU <- merge(gages2withWU, agri, by.x = "STAID", by.y = "STAID", all.x = TRUE)
# simple plotting of 2012 wheat
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = wheat2012)) +
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log")+coord_map("albers", lat0=30, lat1=40)
# simple plotting of 2012 corn
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = corng2012)) +
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log")+coord_map("albers", lat0=30, lat1=40)
# simple plotting of 2012 soy
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = soyb2012)) +
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log")+coord_map("albers", lat0=30, lat1=40)
# simple plotting of 2012 cotton
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = cotton2012)) +
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log")+coord_map("albers", lat0=30, lat1=40)
# simple plotting of 2012 hay
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = hayalf2012)) +
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log")+coord_map("albers", lat0=30, lat1=40)
# simple plotting of 2012 irrigated ag
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = irrig2012)) +
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log")+coord_map("albers", lat0=30, lat1=40)

gages2 <- st_read("all_conus.shp")%>%st_transform("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs ")
transmissivity <- raster("AquiferTransmissivity_Georeff_USGS_spatial.tif")
extract_trans <- as.data.frame(exact_extract(transmissivity, gages2, "mean"))
colnames(extract_trans) <- "transmissivity"
extract_trans$STAID <- gages2$gage_num
hist(extract_trans$transmissivity, 100)

gages2withWU <- merge(gages2withWU, extract_trans, by = "STAID", all.x = TRUE)
# simple plotting of transmissivity for each watershed
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = transmissivity)) +
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log")+coord_map("albers", lat0=30, lat1=40)

############ 
############ 
############ Next steps - (i) calculate rates of change in water use, irrigated ag, crops etc through time 
############ (ii) set thresholds in values of static values and slopes of dynamic values to determine watersheds likely impacted by depletion
############ 
############ 

gages2withWU$linear_rate_of_change_irrigated_area <- NA
gages2withWU$linear_rate_of_change_total_water_use <- NA

for(i in 1:9304){
  # i = 1
  # columns 270 to 276 are for irrigated area 1950 to 2012
  current_IA <- as.numeric(gages2withWU[i,270:276])
  years_IA <- c(1950,1964,1974,1982,1992,2002,2012)
  # columns 201 to 206 are for total water use 1985 to 2010
  current_WU <- as.numeric(gages2withWU[i,201:206])
  years_WU <- c(1985,1990,1995,2000,2005,2010)
  
  current_IA[is.na(current_IA)] <- 0
  current_WU[is.na(current_WU)] <- 0
  
  linear_rate_of_change_irrigated_area <- lm(current_IA ~ years_IA)
  linear_rate_of_change_irrigated_area <- linear_rate_of_change_irrigated_area$coefficients[2]

  linear_rate_of_change_total_water_use <- lm(current_WU ~ years_WU)
  linear_rate_of_change_total_water_use <- linear_rate_of_change_total_water_use$coefficients[2]
  
  gages2withWU$linear_rate_of_change_irrigated_area[i] <- linear_rate_of_change_irrigated_area 
  gages2withWU$linear_rate_of_change_total_water_use[i] <- linear_rate_of_change_total_water_use 
  }

# simple plotting of linear_rate_of_change_irrigated_area
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = linear_rate_of_change_irrigated_area*100*62)) +
  scale_color_viridis(option = "turbo",  direction = 1, trans = "sqrt")+coord_map("albers", lat0=30, lat1=40)

# simple plotting of linear_rate_of_change_total_water_use
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = gages2withWU, aes(x=LNG_GAGE, y=LAT_GAGE, col = (linear_rate_of_change_total_water_use)*25)) +
  scale_color_viridis(option = "turbo",  direction = 1, trans = "sqrt")+coord_map("albers", lat0=30, lat1=40)



