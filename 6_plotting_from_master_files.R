###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
library(googledrive)
library(ggplot2)
library(dplyr)
library(maps)
library(viridis)
library(scales)
library(ggnewscale)
library(mapproj)
library(stringr)
library(dataRetrieval)
library(tidyverse)
# just for doing things old school without creating an R project. Change this to the location you'd like to work in.
base_wd <- "/Users/johnhammond/powell_streamflow_depletion_regional_analysis/"
setwd(base_wd)
dir.create(paste0(base_wd, "master_files"))
setwd(paste0(base_wd, "master_files"))
# access files from google drive
my_url <- "https://drive.google.com/drive/folders/1GS31PaawF0AGGuGnwQ3ThpFSkOd85-XZ"
x <- drive_ls(as_id(my_url))
y <- drive_ls(x$id[2]) # master files folder on google drive
for(i in 1:8){drive_download(y$id[i])} # download master files locally
###
states <- map_data("state")
base_breaks <- function(n = 10){function(x) {axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)}}
### use maser files to plot data
seasonal <- read.csv("seasonal_streamflow_metrics_10102022.csv")
annual <- read.csv("water_year_streamflow_metrics_and_climate_data_10102022.csv")
trends1921 <- read.csv("trends_in_19212020_water_year_streamflow_metrics_and_climate_data_11102022.csv")
trends1951 <- read.csv("trends_in_19512020_water_year_streamflow_metrics_and_climate_data_11102022.csv")
trends1981 <- read.csv("trends_in_19812020_water_year_streamflow_metrics_and_climate_data_11102022.csv")
trends1921_seasonal <- read.csv("trends_in_19212020_seasonal_streamflow_metrics_11102022.csv")
trends1951_seasonal <- read.csv("trends_in_19512020_seasonal_streamflow_metrics_11102022.csv")
trends1981_seasonal <- read.csv("trends_in_19812020_seasonal_streamflow_metrics_11102022.csv")
# getting site info for plotting
gages <- unique(annual$gage)
gages <- str_pad(gages, 8, pad = "0")
gages <- readNWISsite(gages)
gages <- gages[,c("site_no","station_nm","dec_lat_va","dec_long_va","drain_area_va")]
gages$gage <- as.numeric(gages$site_no)
# getting rid of useless columns
annual <- annual[,c(-1,-2,-4)]
seasonal <- seasonal[,c(-1,-2)]
trends1921 <- trends1921[,-1]
trends1951 <- trends1951[,-1]
trends1981 <- trends1981[,-1]
trends1921_seasonal <- trends1921_seasonal[,-1]
trends1951_seasonal <- trends1951_seasonal[,-1]
trends1981_seasonal <- trends1981_seasonal[,-1]
# getting climate from annual file to merge with seasonal file
climate <- annual[,c("gage","wateryear","wy_p_mm","wy_pet_mm","wy_tmean_c","wy_spei_6m","wy_spei_1y","wy_spei_5y","octmar_p_mm","octmar_pet_mm","octmar_tmean_c","octmar_spei_6m","octmar_spei_1y","octmar_spei_5y","octjun_p_mm","octjun_pet_mm","octjun_tmean_c","octjun_spei_6m","octjun_spei_1y","octjun_spei_5y","julsep_p_mm","julsep_pet_mm","julsep_tmean_c","julsep_spei_6m","julsep_spei_1y","julsep_spei_5y")]
seasonal <- merge(seasonal, climate, by = c("gage","wateryear"), all.x = TRUE)
# getting rid of useless trend rows
trends1921 <- subset(trends1921, trends1921$suitable_length == "yes")
trends1951 <- subset(trends1951, trends1951$suitable_length == "yes")
trends1981 <- subset(trends1981, trends1981$suitable_length == "yes")
trends1921_seasonal <- subset(trends1921_seasonal, trends1921_seasonal$suitable_length == "yes")
trends1951_seasonal <- subset(trends1951_seasonal, trends1951_seasonal$suitable_length == "yes")
trends1981_seasonal <- subset(trends1981_seasonal, trends1981_seasonal$suitable_length == "yes")
trends1921 <- trends1921[,-7]
trends1951 <- trends1951[,-7]
trends1981 <- trends1981[,-7]
trends1921_seasonal <- trends1921_seasonal[,-8]
trends1951_seasonal <- trends1951_seasonal[,-8]
trends1981_seasonal <- trends1981_seasonal[,-8]
# getting climate trends in format for plotting / analysis
trends1921_wide <- pivot_wider(trends1921,names_from = X, values_from = c("tau","pval","slope","total_change"))
trends1951_wide <- pivot_wider(trends1951,names_from = X, values_from = c("tau","pval","slope","total_change"))
trends1981_wide <- pivot_wider(trends1981,names_from = X, values_from = c("tau","pval","slope","total_change"))
trends1921_wide_seasonal <- pivot_wider(trends1921_seasonal,names_from = X, values_from = c("tau","pval","slope","total_change"))
trends1951_wide_seasonal <- pivot_wider(trends1951_seasonal,names_from = X, values_from = c("tau","pval","slope","total_change"))
trends1981_wide_seasonal <- pivot_wider(trends1981_seasonal,names_from = X, values_from = c("tau","pval","slope","total_change"))
# preparing long-term median values
longterm_annual_median <- annual %>% group_by(gage) %>% summarise_if(is.numeric, median, na.rm = TRUE)
longterm_seasonal_median <- seasonal %>% group_by(gage, season) %>% summarise_if(is.numeric, median, na.rm = TRUE)
# merging lat long for spatial plotting
longterm_annual_median <- merge(longterm_annual_median, gages, by = "gage", all.x = TRUE)
longterm_seasonal_median <- merge(longterm_seasonal_median, gages, by = "gage", all.x = TRUE)
trends1921_wide <- merge(trends1921_wide, gages, by = "gage", all.x = TRUE)
trends1951_wide <- merge(trends1951_wide, gages, by = "gage", all.x = TRUE)
trends1981_wide <- merge(trends1981_wide, gages, by = "gage", all.x = TRUE)
trends1921_wide_seasonal <- merge(trends1921_wide_seasonal, gages, by = "gage", all.x = TRUE)
trends1951_wide_seasonal <- merge(trends1951_wide_seasonal, gages, by = "gage", all.x = TRUE)
trends1981_wide_seasonal <- merge(trends1981_wide_seasonal, gages, by = "gage", all.x = TRUE)
###################################
###################################
# Plots of long term median values
###################################
###################################

###################################
# annual metrics
###################################

# substitute col = XX, where XX is variable name
# choose from list of variables below:
#"annualflowperwateryear"        
#"annualbaseflowfracperwateryear" "lowest7dayperwateryear"         "medianperwateryear"            
#"lowest7daydateperwateryear"     "daysbelowlowflowthreshannual"   "volumebelowlowflowthreshannual"
#"annual_duration_curve_slope"    "winter_duration_curve_slope"    "spring_duration_curve_slope"   
#"summer_duration_curve_slope"    "fall_duration_curve_slope"      "recess7"                       
#"recess14"                       "recess30"

# using log scale to show variability across all sites
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = longterm_annual_median, aes(x=dec_long_va, y=dec_lat_va, col = annualflowperwateryear)) +
  # lines commented out below could dbe used to plot sites with different symbols for pumping affected sites vs natural sites
  #geom_point(data = longterm_annual_median, aes(x=dec_long_va, y=dec_lat_va, col = median, shape = factor(HCDN.2009), size = factor(HCDN.2009), alpha = factor(HCDN.2009))) +
  #scale_shape_manual(values=c(19, 17))+
  #scale_size_manual(values = c(0.75,2))+
  #scale_alpha_manual(values = c(0.25,1))+
  scale_color_viridis(option = "turbo",  direction = -1, trans = "log", breaks = base_breaks(), labels = function(x)round(x,5))+
  coord_map("albers", lat0=30, lat1=40)

# some variables better plotted on linear scale (recession, slope of the flow duraiton curve)
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = longterm_annual_median, aes(x=dec_long_va, y=dec_lat_va, col = annual_duration_curve_slope)) +
  scale_color_viridis(option = "turbo",  direction = -1)+coord_map("albers", lat0=30, lat1=40)

###################################
# seaonal metrics
###################################

#"seasonalflowperwateryear"            "seasonalbaseflowfracperwateryear"   
#"lowest7dayperwateryearandseason"     "medianperwateryearandseason"        
#"lowest7daydateperwateryearandseason" "daysbelowlowflowthresh"             
#"deficit_mm"

# subset by season
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  theme_void() + 
  geom_point(data = subset(longterm_seasonal_median, longterm_seasonal_median$season == "summer"),
             aes(x=dec_long_va, y=dec_lat_va, col = seasonalbaseflowfracperwateryear)) +
  scale_color_viridis(option = "turbo",  direction = -1)+
  coord_map("albers", lat0=30, lat1=40)


###################################
###################################
# Simple trend plots, not yet accounting for trend significance (e.g. subsetting by p < 0.1)
###################################
###################################

#variables
# "tau_annualflowperwateryear"                 
# "tau_annualbaseflowfracperwateryear"         
# "tau_lowest7dayperwateryear"                 
# "tau_medianperwateryear"                     
# "tau_lowest7daydateperwateryear"             
# "tau_daysbelowlowflowthreshannual"           
# "tau_volumebelowlowflowthreshannual"         
# "tau_annual_duration_curve_slope"            
# "tau_winter_duration_curve_slope"            
# "tau_spring_duration_curve_slope"            
# "tau_summer_duration_curve_slope"            
# "tau_fall_duration_curve_slope"              
# "tau_recess7"                                
# "tau_recess14"                               
# "tau_recess30"
ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
  theme_void() + 
  geom_point(data = trends1981_wide, aes(x=dec_long_va, y=dec_lat_va, col = tau_daysbelowlowflowthreshannual)) +
  scale_color_distiller(palette  = "RdBu",  direction = -1)+coord_map("albers", lat0=30, lat1=40)

ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
  theme_void() + 
  geom_point(data = trends1951_wide, aes(x=dec_long_va, y=dec_lat_va, col = tau_daysbelowlowflowthreshannual)) +
  scale_color_distiller(palette  = "RdBu",  direction = -1)+coord_map("albers", lat0=30, lat1=40)

ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
  theme_void() + 
  geom_point(data = trends1921_wide, aes(x=dec_long_va, y=dec_lat_va, col = tau_daysbelowlowflowthreshannual)) +
  scale_color_distiller(palette  = "RdBu",  direction = -1)+coord_map("albers", lat0=30, lat1=40)

# seasonal

# "tau_seasonalflowperwateryear"                    
# "tau_seasonalbaseflowfracperwateryear"            
# "tau_lowest7dayperwateryearandseason"             
# "tau_medianperwateryearandseason"                 
# "tau_lowest7daydateperwateryearandseason"         
# "tau_daysbelowlowflowthresh"                      
# "tau_deficit_mm" 

ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
  theme_void() + 
  geom_point(data = subset(trends1981_wide_seasonal, trends1981_wide_seasonal$season == "summer"), aes(x=dec_long_va, y=dec_lat_va, col = tau_daysbelowlowflowthresh)) +
  scale_color_distiller(palette  = "RdBu",  direction = -1)+coord_map("albers", lat0=30, lat1=40)

ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
  theme_void() + 
  geom_point(data = subset(trends1951_wide_seasonal, trends1951_wide_seasonal$season == "summer"), aes(x=dec_long_va, y=dec_lat_va, col = tau_daysbelowlowflowthresh)) +
  scale_color_distiller(palette  = "RdBu",  direction = -1)+coord_map("albers", lat0=30, lat1=40)

ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
  theme_void() + 
  geom_point(data = subset(trends1921_wide_seasonal, trends1921_wide_seasonal$season == "summer"), aes(x=dec_long_va, y=dec_lat_va, col = tau_daysbelowlowflowthresh)) +
  scale_color_distiller(palette  = "RdBu",  direction = -1)+coord_map("albers", lat0=30, lat1=40)
###################################
###################################
# example scatter plots
###################################
###################################

# track down code from artificial intermittence plotting - or drought plotting

ggplot(data = trends1951_wide)+geom_point(aes(x=tau_wy_p_mm, y=tau_daysbelowlowflowthreshannual))


  
###################################
###################################
# example time series plots
###################################
###################################




