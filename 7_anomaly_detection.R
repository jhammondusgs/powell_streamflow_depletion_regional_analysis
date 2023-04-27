################################################################################### 
################################################################################### 
################################################################################### 
# Code to experiment with anomaly detection (e.g. sites and years with streamflow that plots away from expected relationships with climate)
# Goals: 
#  1) Use anomalies from expected relationships between climate and annual or seasonal flow metrics to detect streamflow depletion
#     1.1) look at residuals through time to identify likely streamflow depletion
#  2) Look at trends in flow signatures vs climate signatures
# To do:
#  -Determine method for identifying sites/years with anomalous flow vs climate values using annual time series of annual or seasonal flow sigatures
#  -Use classification based on vulnerability to keep track of false positive (site shows anomaly, but anomaly due to factor other than GW pumping) and false negative (site shows no anomaly, but impacted by pumping)
#  -Compute % of flow metric vs climate metric combinations that have anomalous values for each site and each time period. Consider focusing on annual, summer and fall streamflow metrics shown by Lapides et al to be sensitive to depletion.
#  Other ideas:
#  -Implement change point identification methods
#  -consider non-linear trends
#  -Consider rolling trend periods (eg trends for each 30 year period along record) as a way to further analyze the potential occurrence of streamflow depletion once a site is identified as having an anomaly in flow vs climate relations
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
library(tidymodels)
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
### read in master files 
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

################################################################################### 
################################################################################### 
################################################################################### 
# fit curves or linear models to annual flow vs climate data so that annual anomalies 
# can be detected from long-term patterns
################################################################################### 
################################################################################### 
################################################################################### 
# read in a file of regions
regions <- read.csv(paste0(base_wd,"GagesII_CA_MI_KS_072722.csv"))
colnames(regions)[2] <- "STAID"
# assign gages to regions. in the absence of a better classification, just using reference vs non-reference currently
colnames(annual)[17] <- "STAID"
colnames(seasonal)[1] <- "STAID"

annual <- merge(annual, regions, by = "STAID", all.x = TRUE)
seasonal <- merge(seasonal, regions, by = "STAID", all.x = TRUE)

# how many sites in each region classified as reference or non-reference?
 annual_sites <- unique(annual[,c("STAID","Region","Group")])
 annual_sites <- merge(annual_sites, gages, by.x = "STAID", by.y = "gage", all.x = TRUE)
 annual_sites_counts <- annual_sites%>%group_by(Region,Group)%>%summarise(n())
# plot regions and categories within region
 ggplot(data = states)+ geom_polygon(data = states,aes(x = long, y = lat, group = group), fill = NA, color = "black") +
   theme_void() + 
   geom_point(data = annual_sites, aes(x=dec_long_va, y=dec_lat_va, col = Region, shape = Group)) 
 
# fit model for each region using just non-impacted gages
 annual_non_impacted <- subset(annual, annual$Group == "Reference")
# fit a model for each flow and climate metric combination
annual_climate_metrics <- c("wy_p_mm","wy_pet_mm","wy_tmean_c","wy_spei_6m","wy_spei_1y","wy_spei_5y")
annual_flow_metrics <- c("annualflowperwateryear",        
"annualbaseflowfracperwateryear", "lowest7dayperwateryear" ,        "medianperwateryear",            
"lowest7daydateperwateryear",     "daysbelowlowflowthreshannual" ,  "volumebelowlowflowthreshannual",
"annual_duration_curve_slope" ,   "winter_duration_curve_slope" ,   "spring_duration_curve_slope",   
"summer_duration_curve_slope",    "fall_duration_curve_slope" ,     "recess7",                       
"recess14",                       "recess30")
# example for KS, annual metrics
annual_non_impacted_KS <- subset(annual_non_impacted,  annual_non_impacted$Region == "KS")

# fit random forest model to annual flow observations (for each flow metric separately) at non-impacted sites
# using tidymodels code from Sam Zipper at https://www.hydroshare.org/resource/fe9d240438914634abbfdcfa03bed863/, "RandomForestTrends_03_RunModels.R" 

#C:\Users\jhammond\Desktop\Powell_Streamflow_Depletion_September_2022\Regional_analysis\Sam_code\ZipperEtAl_2021-ERL_IntermittencyTrends_DataCode.7z\ZipperEtAl_2021-ERL_IntermittencyTrends_DataCode.7z\ZipperEtAl_2021-ERL_IntermittencyTrends_Data+Code\code
#RandomForestTrends_03_RunModels.R

# run explanatory variables from impacted sites through models generated above

# assess whether residuals for impacted sites tend to be greater and in a different direction than those
# for non-impacted sites

# implement methodology to detect type 1 and type 2 errors


################################################################################### 
################################################################################### 
################################################################################### 
# fit linear models to the trend in flow vs the trends in climate variables so that 
# trend anomalies can be detected from expected trends in climate vs flow relationships 
################################################################################### 
################################################################################### 
################################################################################### 

# assign gages to regions
colnames(trends1921_wide)[1] <- "STAID"
colnames(trends1951_wide)[1] <- "STAID"
colnames(trends1981_wide)[1] <- "STAID"
colnames(trends1921_wide_seasonal)[1] <- "STAID"
colnames(trends1951_wide_seasonal)[1] <- "STAID"
colnames(trends1981_wide_seasonal)[1] <- "STAID"

trends1921_wide <- merge(trends1921_wide, regions, by = "STAID", all.x = TRUE)
trends1951_wide <- merge(trends1951_wide, regions, by = "STAID", all.x = TRUE)
trends1981_wide <- merge(trends1981_wide, regions, by = "STAID", all.x = TRUE)
trends1921_wide_seasonal <- merge(trends1921_wide_seasonal, regions, by = "STAID", all.x = TRUE)
trends1951_wide_seasonal <- merge(trends1951_wide_seasonal, regions, by = "STAID", all.x = TRUE)
trends1981_wide_seasonal <- merge(trends1981_wide_seasonal, regions, by = "STAID", all.x = TRUE)

# fit model for each region using just non-impacted gages
# fit a model for each flow and climate metric combination

# example model would be x=tau_wy_p_mm, y=tau_daysbelowlowflowthreshannual


# run explanatory variables from impacted sites through models generated above

# assess whether residuals for impacted sites tend to be greater and in a different direction than those
# for non-impacted sites


# implement methodology to detect type 1 and type 2 errors


# make summary table showing the number of annual flow vs climate combination with 
# depletion anomalies detected for each region


# make summary table showing the number of flow trend vs climate trend combination with 
# depletion anomalies detected for each region


#### moving beyond this approach at regional scale, when modeling for any individual site, could look for 20 nearest impacted and 20 nearest non-impacted sites within a search distance of 500 miles (for example)