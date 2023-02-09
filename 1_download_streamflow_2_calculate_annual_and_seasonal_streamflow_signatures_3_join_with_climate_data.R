################################################################################### Code to download streamflow data, 
################################################################################### calculate annual and seasonal hydrologic signatures,
################################################################################### obtain antecedent climate data, 
################################################################################### and merge signatures with climate data.
###################################################################################
################################################################################### John Hammond
################################################################################### Powell Streamflw Depletion Workshop
################################################################################### 9/19/2022

library(dplyr)
library(lubridate)
library(dataRetrieval)
library(stringr)

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis")

lowflowsites <- read.csv("GagesII_CA_MI_KS_072722.csv", stringsAsFactors = FALSE )
lowflowsiteids <- str_pad(lowflowsites$site_id_num,8, side = "left", pad = "0")

# there are 165 unique sites from this list

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\streamflow_data")

for(i in seq_along(lowflowsiteids)){
  # i = 1
  try(current <- readNWISdv(siteNumber = lowflowsiteids[i], parameterCd = "00060"))
  try(current$Date <- as.Date(current$Date))
  try(write.csv(current,file=paste0(lowflowsiteids[i],".csv"))) 
}

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################

### Calculate annual metrics only for complete data

library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(zoo)
library(EcoHydRology)
library(foreign)
library(tidyverse)
library(dataRetrieval)

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\streamflow_data")

usgs.files <- list.files(pattern = ".csv")

###########################################################33
for(i in seq_along(usgs.files)){
  
  # i = 42
  # for testing offline: currentareaSqKm = 100
  setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\streamflow_data")
  
  current <- read.csv(usgs.files[i], stringsAsFactors = FALSE)
  currentsite <- gsub(".csv","",usgs.files[i])
  currentarea <- readNWISsite(currentsite)
  currentareaSqKm <- currentarea$drain_area_va*2.58999
  current$mmd <- 1000*(current$X_00060_00003*0.028316847*86400)/(currentareaSqKm*1000000)
  current$month <- month(current$Date)
  current$year <- year(current$Date)
  #current$climyear <- ifelse(current$month<4, current$year-1,current$year)
  current$wateryear <- ifelse(current$month>9, current$year+1,current$year)
  #only want to use approved data
  current <- subset(current, current$X_00060_00003_cd != "P" & current$X_00060_00003_cd != "Pe")
  #going ahead and removing any data for water year 2021 because we might not yet have a complete water year of data
  current <- subset(current, current$wateryear < 2021)
  current$season <- ifelse(current$month > 3 & current$month < 7, "spring",
                           ifelse(current$month > 6 & current$month < 10, "summer",
                                  ifelse(current$month > 9 & current$month, "fall","winter")))
  
  # baseflow separation, one parameter, simple for now, could make more complex later if needed
  baseflow_mm_one_param <- BaseflowSeparation(current$mmd, filter_parameter = 0.925, passes = 3)
  current$baseflow_mmd <- baseflow_mm_one_param$bt
  #current$quickflow_mmd <- baseflow_mm_one_param$qft

  currentcounts <- current %>% group_by(wateryear) %>% summarise(count = sum(!is.na(X_00060_00003)))
  currentcounts <- subset(currentcounts, currentcounts$count>350)
  completeyears <- currentcounts$wateryear
  current <- subset(current, current$wateryear %in% completeyears)
  # total annual flow
  annualflowperwateryear <- current %>% group_by(wateryear) %>% summarise(flowvolume = sum(mmd, na.rm = TRUE))
  # total seasonal flow
  seasonalflowperwateryear <- current %>% group_by(wateryear,season) %>% summarise(flowvolume = sum(mmd, na.rm = TRUE))
  #annual BFI
  annualbaseflowperwateryear <- current %>% group_by(wateryear) %>% summarise(baseflowvolume = sum(baseflow_mmd, na.rm = TRUE))
  annualbaseflowfracperwateryear <- annualbaseflowperwateryear$baseflowvolume/annualflowperwateryear$flowvolume
  annualbaseflowfracperwateryear <- as.data.frame(annualbaseflowfracperwateryear)
  #seasonal BFI
  seasonalbaseflowperwateryear <- current %>% group_by(wateryear,season) %>% summarise(baseflowvolume = sum(baseflow_mmd, na.rm = TRUE))
  seasonalbaseflowfracperwateryear <- seasonalbaseflowperwateryear$baseflowvolume/seasonalflowperwateryear$flowvolume
  seasonalbaseflowfracperwateryear <- as.data.frame(seasonalbaseflowfracperwateryear)
  # rolling 7-day mean of flow for getting minimum 7-day flow
  current$day7mean <- rollmean(current$mmd,7, align = "center", fill = NA)
  # annual minimum 7-day flow
  lowest7dayperwateryear <- current %>% group_by(wateryear) %>% summarise(min = min(day7mean, na.rm = TRUE))
  lowest7dayperwateryear <- lowest7dayperwateryear$min
  # seasonal minimum 7-day flow
  lowest7dayperwateryearandseason <- current %>% group_by(wateryear,season) %>% summarise(min = min(day7mean, na.rm = TRUE))
  lowest7dayperwateryearandseason <- lowest7dayperwateryearandseason$min
  # annual median daily flow
  medianperwateryear <- current %>% group_by(wateryear) %>% summarise(median = median(mmd, na.rm = TRUE))
  medianperwateryear <- medianperwateryear$median
  # seasonal median daily flow
  medianperwateryearandseason <- current %>% group_by(wateryear,season) %>% summarise(median = median(mmd, na.rm = TRUE))
  medianperwateryearandseason <- medianperwateryearandseason$median
  # date of the annual lowest 7-day flow
  lowest7daydateperwateryear <- current %>% group_by(wateryear) %>% slice(which.min(day7mean))
  lowest7daydateperwateryear$yday <- yday(lowest7daydateperwateryear$Date)
  lowest7daydateperwateryear <- lowest7daydateperwateryear[,c("yday")] 
  # date of the seasonal lowest 7-day flow
  lowest7daydateperwateryearandseason <- current %>% group_by(wateryear,season) %>% slice(which.min(day7mean))
  lowest7daydateperwateryearandseason$yday <- yday(lowest7daydateperwateryearandseason$Date)
  lowest7daydateperwateryearandseason <- lowest7daydateperwateryearandseason[,c("yday")] 
  # separate seasonal dataframes
  winter <- subset(current, current$season == "winter")
  spring <- subset(current, current$season == "spring")
  summer <- subset(current, current$season == "summer")
  fall <- subset(current, current$season == "fall")
  # threshold for determining the annual and seasonal number of days below the 10-percent threshold = q90
  annaualper10thresh <- quantile(current$mmd, 0.10, na.rm = TRUE)
  winterper10thresh <- quantile(winter$mmd, 0.10, na.rm = TRUE)
  springper10thresh <- quantile(spring$mmd, 0.10, na.rm = TRUE)
  summerper10thresh <- quantile(summer$mmd, 0.10, na.rm = TRUE)
  fallper10thresh <- quantile(fall$mmd, 0.10, na.rm = TRUE)
  # threshold to use when looking at recessions not to use peaky flows
   per70thresh <- quantile(current$mmd, 0.70, na.rm = TRUE)
  # calculate low flow duration (days below 10%), and accumulated flow deficit volume below 10% annually and seasonally
  current$belowlowflowthresh <- ifelse(current$mmd < annaualper10thresh, 1,0)
  daysbelowlowflowthreshannual <- current %>% group_by(wateryear) %>% summarise(daysbelowlowflowthresh = sum(belowlowflowthresh, na.rm = TRUE))
  daysbelowlowflowthreshannual <- daysbelowlowflowthreshannual[,"daysbelowlowflowthresh"]
  current$flowdeparturebelowthreshold <- ifelse(current$mmd < annaualper10thresh, annaualper10thresh-current$mmd,0)
  volumebelowlowflowthreshannual <- current %>% group_by(wateryear) %>% summarise(deficit_mm = sum(flowdeparturebelowthreshold, na.rm = TRUE))
  volumebelowlowflowthreshannual <- volumebelowlowflowthreshannual[,"deficit_mm"]
  # winter
  winter$belowlowflowthresh <- ifelse(winter$mmd < annaualper10thresh, 1,0)
  daysbelowlowflowthreshwinter <- winter %>% group_by(wateryear) %>% summarise(daysbelowlowflowthresh = sum(belowlowflowthresh, na.rm = TRUE))
  #daysbelowlowflowthreshwinter <- daysbelowlowflowthreshwinter[,"daysbelowlowflowthresh"]
  winter$flowdeparturebelowthreshold <- ifelse(winter$mmd < annaualper10thresh, annaualper10thresh-winter$mmd,0)
  volumebelowlowflowthreshwinter <- winter %>% group_by(wateryear) %>% summarise(deficit_mm = sum(flowdeparturebelowthreshold, na.rm = TRUE))
  #volumebelowlowflowthreshwinter <- volumebelowlowflowthreshwinter[,"deficit_mm"]
  # spring
  spring$belowlowflowthresh <- ifelse(spring$mmd < annaualper10thresh, 1,0)
  daysbelowlowflowthreshspring <- spring %>% group_by(wateryear) %>% summarise(daysbelowlowflowthresh = sum(belowlowflowthresh, na.rm = TRUE))
  #daysbelowlowflowthreshspring <- daysbelowlowflowthreshspring[,"daysbelowlowflowthresh"]
  spring$flowdeparturebelowthreshold <- ifelse(spring$mmd < annaualper10thresh, annaualper10thresh-spring$mmd,0)
  volumebelowlowflowthreshspring <- spring %>% group_by(wateryear) %>% summarise(deficit_mm = sum(flowdeparturebelowthreshold, na.rm = TRUE))
  #volumebelowlowflowthreshspring <- volumebelowlowflowthreshspring[,"deficit_mm"]
  # summer
  summer$belowlowflowthresh <- ifelse(summer$mmd < annaualper10thresh, 1,0)
  daysbelowlowflowthreshsummer <- summer %>% group_by(wateryear) %>% summarise(daysbelowlowflowthresh = sum(belowlowflowthresh, na.rm = TRUE))
  #daysbelowlowflowthreshsummer <- daysbelowlowflowthreshsummer[,"daysbelowlowflowthresh"]
  summer$flowdeparturebelowthreshold <- ifelse(summer$mmd < annaualper10thresh, annaualper10thresh-summer$mmd,0)
  volumebelowlowflowthreshsummer <- summer %>% group_by(wateryear) %>% summarise(deficit_mm = sum(flowdeparturebelowthreshold, na.rm = TRUE))
  #volumebelowlowflowthreshsummer <- volumebelowlowflowthreshsummer[,"deficit_mm"]
  # fall
  fall$belowlowflowthresh <- ifelse(fall$mmd < annaualper10thresh, 1,0)
  daysbelowlowflowthreshfall <- fall %>% group_by(wateryear) %>% summarise(daysbelowlowflowthresh = sum(belowlowflowthresh, na.rm = TRUE))
  #daysbelowlowflowthreshfall <- daysbelowlowflowthreshfall[,"daysbelowlowflowthresh"]
  fall$flowdeparturebelowthreshold <- ifelse(fall$mmd < annaualper10thresh, annaualper10thresh-fall$mmd,0)
  volumebelowlowflowthreshfall <- fall %>% group_by(wateryear) %>% summarise(deficit_mm = sum(flowdeparturebelowthreshold, na.rm = TRUE))
  #volumebelowlowflowthreshfall <- volumebelowlowflowthreshfall[,"deficit_mm"]
  # putting the seasonal datasets together
  daysbelowlowflowthreshwinter$season <- "winter"
  volumebelowlowflowthreshwinter$season <- "winter"
  daysbelowlowflowthreshspring$season <- "spring"
  volumebelowlowflowthreshspring$season <- "spring"
  daysbelowlowflowthreshsummer$season <- "summer"
  volumebelowlowflowthreshsummer$season <- "summer"
  daysbelowlowflowthreshfall$season <- "fall"
  volumebelowlowflowthreshfall$season <- "fall"
  
  daysbelowlowflowthreshseasonal <- rbind(daysbelowlowflowthreshwinter, daysbelowlowflowthreshspring, daysbelowlowflowthreshsummer, daysbelowlowflowthreshfall)
  volumebelowlowflowthreshseasonal <- rbind(volumebelowlowflowthreshwinter, volumebelowlowflowthreshspring, volumebelowlowflowthreshsummer, volumebelowlowflowthreshfall)
  
  fillwithannualvalues <- as.data.frame(matrix(data= NA, nrow = nrow(daysbelowlowflowthreshannual), ncol = 8))
  colnames(fillwithannualvalues) <- c("annual_duration_curve_slope","winter_duration_curve_slope","spring_duration_curve_slope",
                                      "summer_duration_curve_slope","fall_duration_curve_slope","recess7","recess14","recess30")
  
  for(y in 1:length(completeyears)){
    #y = 2
    currentyear <- subset(current,current$wateryear == completeyears[y])
    try(prioryear <- subset(current,current$wateryear == completeyears[y-1]))
    try(prioryear$yday <- yday(prioryear$Date))
    try(prioryear <- subset(prioryear, prioryear$yday > 242 & prioryear$yday < 274))
    try(prioryear$wyday <- ifelse(prioryear$yday > 273, prioryear$yday - 273, prioryear$yday + 92))
    
    wintercurrentyear <- subset(winter,winter$wateryear == completeyears[y])
    springcurrentyear <- subset(spring,spring$wateryear == completeyears[y])
    summercurrentyear <- subset(summer,summer$wateryear == completeyears[y])
    fallcurrentyear <- subset(fall,fall$wateryear == completeyears[y])
    
    currentyear$yday <- yday(currentyear$Date)
    currentyear$wyday <- ifelse(currentyear$yday > 273, currentyear$yday - 273, currentyear$yday + 92)
    
    # setting to NA before calculating in case calculation fails
    annual_duration_curve_slope <- NA
    winter_duration_curve_slope <- NA
    spring_duration_curve_slope <- NA
    summer_duration_curve_slope <- NA
    fall_duration_curve_slope <- NA
    
    # FDC using all flows, calculating slope of FDC after removing under 10 and above 90
    annual_duration_curve <- currentyear %>% mutate(my_rank = 1- percent_rank(mmd)) %>% mutate(my_rank = if_else(my_rank == 1, 0.9999, my_rank))
    annual_duration_curve <- subset(annual_duration_curve, annual_duration_curve$my_rank>0.0999 & annual_duration_curve$my_rank <0.900001)
    try(annual_duration_curve_slope <- lm(mmd~my_rank, data = annual_duration_curve))
        try(annual_duration_curve_slope <- coef(annual_duration_curve_slope)[2])
    # FDC using winter flows, calculating slope of FDC after removing under 10 and above 90
    winter_duration_curve <- wintercurrentyear %>% mutate(my_rank = 1- percent_rank(mmd)) %>% mutate(my_rank = if_else(my_rank == 1, 0.9999, my_rank))
    winter_duration_curve <- subset(winter_duration_curve, winter_duration_curve$my_rank>0.0999 & winter_duration_curve$my_rank <0.900001)
    try(winter_duration_curve_slope <- lm(mmd~my_rank, data = winter_duration_curve))
        try(winter_duration_curve_slope <- coef(winter_duration_curve_slope)[2])
    # FDC using spring flows, calculating slope of FDC after removing under 10 and above 90
    spring_duration_curve <- springcurrentyear %>% mutate(my_rank = 1- percent_rank(mmd)) %>% mutate(my_rank = if_else(my_rank == 1, 0.9999, my_rank))
    spring_duration_curve <- subset(spring_duration_curve, spring_duration_curve$my_rank>0.0999 & spring_duration_curve$my_rank <0.900001)
    try(spring_duration_curve_slope <- lm(mmd~my_rank, data = spring_duration_curve))
        try(spring_duration_curve_slope <- coef(spring_duration_curve_slope)[2])
    # FDC using summer flows, calculating slope of FDC after removing under 10 and above 90
    summer_duration_curve <- summercurrentyear %>% mutate(my_rank = 1- percent_rank(mmd)) %>% mutate(my_rank = if_else(my_rank == 1, 0.9999, my_rank))
    summer_duration_curve <- subset(summer_duration_curve, summer_duration_curve$my_rank>0.0999 & summer_duration_curve$my_rank <0.900001)
    try(summer_duration_curve_slope <- lm(mmd~my_rank, data = summer_duration_curve))
        try(summer_duration_curve_slope <- coef(summer_duration_curve_slope)[2])
    # FDC using fall flows, calculating slope of FDC after removing under 10 and above 90
    fall_duration_curve <- fallcurrentyear %>% mutate(my_rank = 1- percent_rank(mmd)) %>% mutate(my_rank = if_else(my_rank == 1, 0.9999, my_rank))
    fall_duration_curve <- subset(fall_duration_curve, fall_duration_curve$my_rank>0.0999 & fall_duration_curve$my_rank <0.900001)
    try(fall_duration_curve_slope <- lm(mmd~my_rank, data = fall_duration_curve))
        try(fall_duration_curve_slope <- coef(fall_duration_curve_slope)[2])
    
    # which is date of minimum 7 day flow? now count back 30 days and calculate recession   
    
    lowest7daythisyear <- currentyear %>% slice(which.min(day7mean))
    daytocountbackfrom <- as.numeric(lowest7daythisyear$wyday)
    
    # testing to see if there are 30 days in the water year leading up to the lowest 7-day flow
    firstday30 <- daytocountbackfrom-30
    
    try(if(firstday30>0){ # only calculate recession if not affected by split between two water years, or if data for prior year available
      
      recessionperiod30 <- currentyear[firstday30:daytocountbackfrom,]
      firstday14 <- daytocountbackfrom-14
      recessionperiod14 <- currentyear[firstday14:daytocountbackfrom,]
      firstday7 <- daytocountbackfrom-7
      recessionperiod7 <- currentyear[firstday7:daytocountbackfrom,]
      
      recessionperiod30$mmd <- ifelse(recessionperiod30$mmd<0.00001,0.00001,recessionperiod30$mmd)
      recessionperiod14$mmd <- ifelse(recessionperiod14$mmd<0.00001,0.00001,recessionperiod14$mmd)
      recessionperiod7$mmd <- ifelse(recessionperiod7$mmd<0.00001,0.00001,recessionperiod7$mmd)
      # removing peaks that could obscure calculation of recession rate to low flows
      recessionperiod30 <- subset(recessionperiod30, recessionperiod30$mmd < per70thresh)
      recessionperiod14 <- subset(recessionperiod14, recessionperiod14$mmd < per70thresh)
      recessionperiod7 <- subset(recessionperiod7, recessionperiod7$mmd < per70thresh)
      
      recessionperiod30$lnq <- log(recessionperiod30$mmd)
      recessionperiod14$lnq <- log(recessionperiod14$mmd)
      recessionperiod7$lnq <- log(recessionperiod7$mmd)
      
      recess30 <- lm(lnq ~ wyday, data = recessionperiod30)
      recess30 <- recess30$coefficients[2]
      recess14 <- lm(lnq ~ wyday, data = recessionperiod14)
      recess14 <- recess14$coefficients[2]
      recess7 <- lm(lnq ~ wyday, data = recessionperiod7)
      recess7 <- recess7$coefficients[2]
      
      
      
    } else if (firstday30 < 1 & length(prioryear$X_00060_00003)>29) {
     
      
      currentyear <- rbind(currentyear, prioryear)
      
      recessionperiod30 <- currentyear[(firstday30+30):(daytocountbackfrom+30),]
      firstday14 <- daytocountbackfrom-14
      recessionperiod14 <- currentyear[(firstday14+30):(daytocountbackfrom+30),]
      firstday7 <- daytocountbackfrom-7
      recessionperiod7 <- currentyear[(firstday7+30):(daytocountbackfrom+30),]
      
      recessionperiod30$mmd <- ifelse(recessionperiod30$mmd<0.00001,0.00001,recessionperiod30$mmd)
      recessionperiod14$mmd <- ifelse(recessionperiod14$mmd<0.00001,0.00001,recessionperiod14$mmd)
      recessionperiod7$mmd <- ifelse(recessionperiod7$mmd<0.00001,0.00001,recessionperiod7$mmd)
      # removing peaks that could obscure calculation of recession rate to low flows
      recessionperiod30 <- subset(recessionperiod30, recessionperiod30$mmd < per70thresh)
      recessionperiod14 <- subset(recessionperiod14, recessionperiod14$mmd < per70thresh)
      recessionperiod7 <- subset(recessionperiod7, recessionperiod7$mmd < per70thresh)
      
      recessionperiod30$lnq <- log(recessionperiod30$mmd)
      recessionperiod14$lnq <- log(recessionperiod14$mmd)
      recessionperiod7$lnq <- log(recessionperiod7$mmd)
      
      recess30 <- lm(lnq ~ wyday, data = recessionperiod30)
      recess30 <- recess30$coefficients[2]
      recess14 <- lm(lnq ~ wyday, data = recessionperiod14)
      recess14 <- recess14$coefficients[2]
      recess7 <- lm(lnq ~ wyday, data = recessionperiod7)
      recess7 <- recess7$coefficients[2]
      
       ######### ADD MORE COLUMNS TO OUTPUT
      
    } else{
      recess30 <- NA
      recess14 <- NA
      recess7 <- NA
    })
    

    try(fillvalues <- c(annual_duration_curve_slope,winter_duration_curve_slope,spring_duration_curve_slope,
                        summer_duration_curve_slope,fall_duration_curve_slope,recess7,recess14,recess30))
    
    try(fillwithannualvalues[y,] <- fillvalues)
    
  
  }
  
  try(fillwithannualvalues$wateryear <- completeyears)

  # merge in the seaosnal days below and volume below threshold
  
 try(allannualoutput <- cbind(annualflowperwateryear, annualbaseflowfracperwateryear, lowest7dayperwateryear, medianperwateryear, lowest7daydateperwateryear, daysbelowlowflowthreshannual, volumebelowlowflowthreshannual))
  try(colnames(allannualoutput) <- c("wateryear","annualflowperwateryear", "annualbaseflowfracperwateryear", 
                                       "lowest7dayperwateryear", "medianperwateryear", "lowest7daydateperwateryear", 
                                       "daysbelowlowflowthreshannual", "volumebelowlowflowthreshannual"))
  
  # then add special annual calculations from yearly loops
  
  try(allannualoutput <- merge(allannualoutput, fillwithannualvalues, by = "wateryear"))
  
  try(allseasonaloutput <- cbind(seasonalflowperwateryear, seasonalbaseflowfracperwateryear, lowest7dayperwateryearandseason, medianperwateryearandseason, lowest7daydateperwateryearandseason))
  try(colnames(allseasonaloutput) <- c("wateryear","season","seasonalflowperwateryear", "seasonalbaseflowfracperwateryear", 
                                       "lowest7dayperwateryearandseason", "medianperwateryearandseason", "lowest7daydateperwateryearandseason"))
  # then merge seasonal duration and deficits
  
  try(allseasonaloutput <- merge(allseasonaloutput, daysbelowlowflowthreshseasonal, by = c("wateryear","season")))
  try(allseasonaloutput <- merge(allseasonaloutput, volumebelowlowflowthreshseasonal, by = c("wateryear","season")))
  
 try(allannualoutput$gage <- currentsite)
 try(allseasonaloutput$gage <- currentsite)
  

  setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\streamflow_metrics")
  
  try(write.csv(allannualoutput, paste0("annual_metrics_",usgs.files[i])))
  try(write.csv(allseasonaloutput, paste0("seasonal_metrics_",usgs.files[i])))
  
}



####################################
####################################
####################################
####################################
####################################

######################## climate data pull from existing files, then compute SPEI

# do this one streamflow metric file at a time and get antecedent climate for different periods

 
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(SPEI)

setwd("C:\\Users\\jhammond\\Desktop\\Rebekah_Manning\\MWBM_data")

pet <- fread("MWBM_PET_mm_all_CONUS_gages2_climgrid.csv")
p <- fread("MWBM_PRCP_mm_all_CONUS_gages2_climgrid.csv")
tmean <- fread("MWBM_Tmean_C_all_CONUS_gages2_climgrid.csv")

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\streamflow_metrics")

usgs.files <- list.files(pattern = "annual_metrics")

for(i in seq_along(usgs.files)){
  # i = 1
  setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\streamflow_metrics")
  
  current <- read.csv(usgs.files[i], stringsAsFactors = FALSE)
  usgssite <- unique(as.character(current$gage))
  
  p_sub <- p %>% select(usgssite, date)
  pet_sub <- pet %>% select(usgssite)
  tmean_sub <- tmean %>% select(usgssite)
  
  climate <- cbind(p_sub, pet_sub, tmean_sub)
  climate <- climate[,c(2,1,3,4)]
  colnames(climate) <- c("Date","p_mm","pet_mm","tmean_c")
  climate$year <- year(climate$Date)
  climate$month <- month(climate$Date)
  climate$wateryear <- ifelse(climate$month>9, climate$year+1,climate$year)
  
  # calculate SPEI
  
  climate$P_minus_PET <- climate$p_mm - climate$pet_mm
  climate$P_minus_PET <- ifelse(is.na(climate$P_minus_PET),0,climate$P_minus_PET) # should not be any missing data, but if there is, need to feed function values in order to run
  spei6month <- spei(climate[,'P_minus_PET'], 6) # antecedent 6 months
  spei12month <- spei(climate[,'P_minus_PET'], 12) # antecedent 6 months
  spei60month <- spei(climate[,'P_minus_PET'], 60) # antecedent 6 months
  climate$spei6month <- spei6month$fitted
  climate$spei12month <- spei12month$fitted # 1 year
  climate$spei60month <- spei60month$fitted # 5 years
  
  setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\streamflow_metrics_with_climate")
  
  # aggregate climate to several antecedent periods
october_to_march_climate <- subset(climate, climate$month > 9 | climate$month < 4)
october_to_march_climate <- october_to_march_climate %>% group_by(wateryear) %>% summarise(octmar_p_mm <- sum(p_mm, na.rm = TRUE),
                                                                                             octmar_pet_mm <- sum(pet_mm, na.rm = TRUE),
                                                                                             octmar_tmean_c <- mean(tmean_c, na.rm = TRUE),
                                                                                             octmar_spei_6m <- mean(spei6month, na.rm = TRUE),
                                                                                             octmar_spei_1y <- mean(spei12month, na.rm = TRUE),
                                                                                             octmar_spei_5y <- mean(spei60month, na.rm = TRUE))

october_to_june_climate <- subset(climate, climate$month > 9 | climate$month < 7)
october_to_june_climate <- october_to_june_climate %>% group_by(wateryear) %>% summarise(octjun_p_mm <- sum(p_mm, na.rm = TRUE),
                                                                                           octjun_pet_mm <- sum(pet_mm, na.rm = TRUE),
                                                                                           octjun_tmean_c <- mean(tmean_c, na.rm = TRUE),
                                                                                           octjun_spei_6m <- mean(spei6month, na.rm = TRUE),
                                                                                           octjun_spei_1y <- mean(spei12month, na.rm = TRUE),
                                                                                           octjun_spei_5y <- mean(spei60month, na.rm = TRUE))

water_year_climate <- climate %>% group_by(wateryear) %>% summarise(wy_p_mm <- sum(p_mm, na.rm = TRUE),
                                                                                    wy_pet_mm <- sum(pet_mm, na.rm = TRUE),
                                                                                    wy_tmean_c <- mean(tmean_c, na.rm = TRUE),
                                                                                    wy_spei_6m <- mean(spei6month, na.rm = TRUE),
                                                                                    wy_spei_1y <- mean(spei12month, na.rm = TRUE),
                                                                                    wy_spei_5y <- mean(spei60month, na.rm = TRUE))
  
july_to_september_climate <- subset(climate, climate$month > 6 & climate$month < 10)
july_to_september_climate <- july_to_september_climate %>% group_by(wateryear) %>% summarise(julsep_p_mm <- sum(p_mm, na.rm = TRUE),
                                                                                         julsep_pet_mm <- sum(pet_mm, na.rm = TRUE),
                                                                                         julsep_tmean_c <- mean(tmean_c, na.rm = TRUE),
                                                                                         julsep_spei_6m <- mean(spei6month, na.rm = TRUE),
                                                                                         julsep_spei_1y <- mean(spei12month, na.rm = TRUE),
                                                                                         julsep_spei_5y <- mean(spei60month, na.rm = TRUE))
  # merge with streamflow metrics
  
current_with_climate <- merge(current, water_year_climate, by = "wateryear")
current_with_climate <- merge(current_with_climate, october_to_march_climate, by = "wateryear")
current_with_climate <- merge(current_with_climate, october_to_june_climate, by = "wateryear")
current_with_climate <- merge(current_with_climate, july_to_september_climate, by = "wateryear")

colnames(current_with_climate)[19:42] <- c("wy_p_mm"               , "wy_pet_mm" ,          
                                           "wy_tmean_c"        , "wy_spei_6m"     ,
                                           "wy_spei_1y"    , "wy_spei_5y"    ,
                                           "octmar_p_mm"           , "octmar_pet_mm",       
                                           "octmar_tmean_c"    , "octmar_spei_6m" ,
                                           "octmar_spei_1y", "octmar_spei_5y",
                                           "octjun_p_mm"           , "octjun_pet_mm",       
                                           "octjun_tmean_c"    , "octjun_spei_6m" ,
                                           "octjun_spei_1y", "octjun_spei_5y",
                                           "julsep_p_mm"           , "julsep_pet_mm",       
                                           "julsep_tmean_c"    , "julsep_spei_6m" ,
                                           "julsep_spei_1y", "julsep_spei_5y")
# write output to new folder
  write.csv( current_with_climate , paste0(usgs.files[i],"_with_climate.csv"))
  
}

