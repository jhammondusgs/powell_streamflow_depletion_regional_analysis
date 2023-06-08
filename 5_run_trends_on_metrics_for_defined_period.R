# code below not updated to run for any user using github and google drive yet
# you can manually download master files from google drive and change working directory below to rerun trends if needed
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
### Run trends on complete data for periods 1951-2020, 1951-2020, 1981-2020
# Want less than 10% of years missing to include in trend analysis: 91 years record required for 1951-2020, 64 for 1951-2020, 37 for 1981-2020
# First, trends for 1951-2020
library(googledrive)
library(Kendall)
library(trend)
library(plyr)
library(PerformanceAnalytics)
library(arrayhelpers)
library(lubridate)
library(zyp)
library(stringr)
library(data.table)
library(dplyr)
library(jsonlite)

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
# First running trends in annual metrics
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
#1921-2020 first

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\master_files_NATIONAL")
sw <- read.csv("water_year_streamflow_metrics_and_climate_data_NATIONAL_042123.csv")
sw$gage <- as.character(sw$gage)
swsites <- unique(sw$gage)
swsites <- as.character(swsites)
level = 0.1
period_length = 100 # less than 10% missing years
###################### prepare geoknife ecoregion monthly data
setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19212020_NATIONAL")
for(i in seq_along(swsites)){
  # i = 1
  current <- subset(sw, sw$gage == swsites[i])
  current[mapply(is.infinite, current)] <- NA
  current <- current[,-20]
  try(current <- subset(current, current$wateryear>1920 & current$wateryear<2021))
  # change this based on trend period ie 1950 to 2018, 1950 - 2018
  years <- as.data.frame(1921:2020)
  nathresh <- 10 # less than 10% missing years
  # rest does not need changing
  colnames(years) <- "wateryear"
  current <- merge(current,years, by = "wateryear", all = TRUE)
  results <- data.frame(matrix(NA, nrow = 39, ncol = 6))
  rownames(results) <- colnames(current)[c(5:43)]
  colnames(results) <- c("gage","tau","pval","slope","total_change","suitable_length")
  
  for(c in 1:39){
    # c = 1
    currentcolumnname <- colnames(current)[c+4]
    currentcolumn <- as.data.frame(current[,c+4])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current$wateryear
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,suitable_trend_length)
      results[(c),] <- output
    } 
  }
  write.csv(results, paste(swsites[i],".trend.summary.csv",sep = ""))}

####################################################################################
####################################################################################
####################################################################################
####################################################################################
# Next, trends for 1951-2020

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\master_files_NATIONAL")
sw <- read.csv("water_year_streamflow_metrics_and_climate_data_NATIONAL_042123.csv")
sw$gage <- as.character(sw$gage)
swsites <- unique(sw$gage)
swsites <- as.character(swsites)
level = 0.1
period_length = 70 # less than 10% missing years
###################### prepare geoknife ecoregion monthly data
setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19512020_NATIONAL")
for(i in seq_along(swsites)){
  # i = 1
  current <- subset(sw, sw$gage == swsites[i])
  current[mapply(is.infinite, current)] <- NA
  current <- current[,-20]
  try(current <- subset(current, current$wateryear>1950 & current$wateryear<2021))
  # change this based on trend period ie 1950 to 2018, 1950 - 2018
  years <- as.data.frame(1951:2020)
  nathresh <- 7 # less than 10% missing years
  # rest does not need changing
  colnames(years) <- "wateryear"
  current <- merge(current,years, by = "wateryear", all = TRUE)
  results <- data.frame(matrix(NA, nrow = 39, ncol = 6))
  rownames(results) <- colnames(current)[c(5:43)]
  colnames(results) <- c("gage","tau","pval","slope","total_change","suitable_length")
  
  for(c in 1:39){
    # c = 1
    currentcolumnname <- colnames(current)[c+4]
    currentcolumn <- as.data.frame(current[,c+4])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current$wateryear
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,suitable_trend_length)
      results[(c),] <- output
    } 
  }
  write.csv(results, paste(swsites[i],".trend.summary.csv",sep = ""))}

####################################################################################
####################################################################################
####################################################################################
####################################################################################
# Finally, trends for 1981-2020

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\master_files_NATIONAL")
sw <- read.csv("water_year_streamflow_metrics_and_climate_data_NATIONAL_042123.csv")
sw$gage <- as.character(sw$gage)
swsites <- unique(sw$gage)
swsites <- as.character(swsites)
level = 0.1
period_length = 40 # less than 10% missing years
###################### prepare geoknife ecoregion monthly data
setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19812020_NATIONAL")
for(i in seq_along(swsites)){
  # i = 1
  current <- subset(sw, sw$gage == swsites[i])
  current[mapply(is.infinite, current)] <- NA
  current <- current[,-20]
  try(current <- subset(current, current$wateryear>1980 & current$wateryear<2021))
  # change this based on trend period ie 1950 to 2018, 1950 - 2018
  years <- as.data.frame(1981:2020)
  nathresh <- 4 # less than 10% missing years
  # rest does not need changing
  colnames(years) <- "wateryear"
  current <- merge(current,years, by = "wateryear", all = TRUE)
  results <- data.frame(matrix(NA, nrow = 39, ncol = 6))
  rownames(results) <- colnames(current)[c(5:43)]
  colnames(results) <- c("gage","tau","pval","slope","total_change","suitable_length")
  
  for(c in 1:39){
    # c = 1
    currentcolumnname <- colnames(current)[c+4]
    currentcolumn <- as.data.frame(current[,c+4])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current$wateryear
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,suitable_trend_length)
      results[(c),] <- output
    } 
  }
  write.csv(results, paste(swsites[i],".trend.summary.csv",sep = ""))}

####################################################################################
####################################################################################
####################################################################################
####################################################################################
# merge individual site trend files
setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19212020_NATIONAL")
all1921 <- list.files(pattern = ".csv")
alldata1921 <- do.call(rbind, lapply(all1921, read.csv))

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19512020_NATIONAL")
all1951 <- list.files(pattern = ".csv")
alldata1951 <- do.call(rbind, lapply(all1951, read.csv))

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19812020_NATIONAL")
all1981 <- list.files(pattern = ".csv")
alldata1981 <- do.call(rbind, lapply(all1981, read.csv))

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\master_files_NATIONAL")

write.csv(alldata1921, "trends_in_19212020_water_year_streamflow_metrics_and_climate_data_NATIONAL_042123.csv")
write.csv(alldata1951, "trends_in_19512020_water_year_streamflow_metrics_and_climate_data_NATIONAL_042123.csv")
write.csv(alldata1981, "trends_in_19812020_water_year_streamflow_metrics_and_climate_data_NATIONAL_042123.csv")

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
# Second running trends in seasonal metrics
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
# 1921-2020
setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\master_files_NATIONAL")
sw <- read.csv("seasonal_streamflow_metrics_NATIONAL_042123.csv")
sw$gage <- as.character(sw$gage)
swsites <- unique(sw$gage)
swsites <- as.character(swsites)
level = 0.1
period_length = 100 # less than 10% missing years
###################### prepare geoknife ecoregion monthly data
setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19212020_seasonal_NATIONAL")
for(i in seq_along(swsites)){
  # i = 1
  current <- subset(sw, sw$gage == swsites[i])
  current[mapply(is.infinite, current)] <- NA
  current <- current[,c(-1,-2,-12)]
  try(current <- subset(current, current$wateryear>1920 & current$wateryear<2021))
  # change this based on trend period ie 1950 to 2018, 1950 - 2018
  years <- as.data.frame(1921:2020)
  nathresh <- 10 # less than 10% missing years
  # rest does not need changing
  colnames(years) <- "wateryear"
  current <- merge(current,years, by = "wateryear", all = TRUE)
  results <- data.frame(matrix(NA, nrow = 7, ncol = 7))
  rownames(results) <- colnames(current)[c(3:9)]
  colnames(results) <- c("gage","tau","pval","slope","total_change","season","suitable_length")
  
  results_winter <- results
  results_spring <- results
  results_summer <- results
  results_fall <- results
  
  current_winter <- subset(current, current$season == "winter")
  current_spring <- subset(current, current$season == "spring")
  current_summer <- subset(current, current$season == "summer")
  current_fall <- subset(current, current$season == "fall")
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_winter)[c+2]
    currentcolumn <- as.data.frame(current_winter[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_winter$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"winter",suitable_trend_length)
      results_winter[(c),] <- output
    } 
  }
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_spring)[c+2]
    currentcolumn <- as.data.frame(current_spring[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_spring$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"spring",suitable_trend_length)
      results_spring[(c),] <- output
    } 
  }
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_summer)[c+2]
    currentcolumn <- as.data.frame(current_summer[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_summer$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"summer",suitable_trend_length)
      results_summer[(c),] <- output
    } 
  }
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_fall)[c+2]
    currentcolumn <- as.data.frame(current_fall[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_fall$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"fall",suitable_trend_length)
      results_fall[(c),] <- output
    } 
  }
  
  results <- rbind(results_winter,results_spring, results_summer, results_fall)
  write.csv(results, paste(swsites[i],".trend.summary.csv",sep = ""))}


##############################################################################
############################################################################## 1951 2020
##############################################################################


setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\master_files_NATIONAL")
sw <- read.csv("seasonal_streamflow_metrics_NATIONAL_042123.csv")
sw$gage <- as.character(sw$gage)
swsites <- unique(sw$gage)
swsites <- as.character(swsites)
level = 0.1
period_length = 70 # less than 10% missing years
###################### prepare geoknife ecoregion monthly data
setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19512020_seasonal_NATIONAL")
for(i in seq_along(swsites)){
  # i = 1
  current <- subset(sw, sw$gage == swsites[i])
  current[mapply(is.infinite, current)] <- NA
  current <- current[,c(-1,-2,-12)]
  try(current <- subset(current, current$wateryear>1950 & current$wateryear<2021))
  # change this based on trend period ie 1950 to 2018, 1950 - 2018
  years <- as.data.frame(1951:2020)
  nathresh <- 7 # less than 10% missing years
  # rest does not need changing
  colnames(years) <- "wateryear"
  current <- merge(current,years, by = "wateryear", all = TRUE)
  results <- data.frame(matrix(NA, nrow = 7, ncol = 7))
  rownames(results) <- colnames(current)[c(3:9)]
  colnames(results) <- c("gage","tau","pval","slope","total_change","season","suitable_length")
  
  results_winter <- results
  results_spring <- results
  results_summer <- results
  results_fall <- results
  
  current_winter <- subset(current, current$season == "winter")
  current_spring <- subset(current, current$season == "spring")
  current_summer <- subset(current, current$season == "summer")
  current_fall <- subset(current, current$season == "fall")
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_winter)[c+2]
    currentcolumn <- as.data.frame(current_winter[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_winter$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"winter",suitable_trend_length)
      results_winter[(c),] <- output
    } 
  }
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_spring)[c+2]
    currentcolumn <- as.data.frame(current_spring[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_spring$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"spring",suitable_trend_length)
      results_spring[(c),] <- output
    } 
  }
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_summer)[c+2]
    currentcolumn <- as.data.frame(current_summer[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_summer$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"summer",suitable_trend_length)
      results_summer[(c),] <- output
    } 
  }
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_fall)[c+2]
    currentcolumn <- as.data.frame(current_fall[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_fall$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"fall",suitable_trend_length)
      results_fall[(c),] <- output
    } 
  }
  
  results <- rbind(results_winter,results_spring, results_summer, results_fall)
  write.csv(results, paste(swsites[i],".trend.summary.csv",sep = ""))}

##############################################################################
############################################################################## 1981 2020
##############################################################################

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\master_files_NATIONAL")
sw <- read.csv("seasonal_streamflow_metrics_NATIONAL_042123.csv")
sw$gage <- as.character(sw$gage)
swsites <- unique(sw$gage)
swsites <- as.character(swsites)
level = 0.1
period_length = 40 # less than 10% missing years
###################### prepare geoknife ecoregion monthly data
setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19812020_seasonal_NATIONAL")
for(i in seq_along(swsites)){
  # i = 1
  current <- subset(sw, sw$gage == swsites[i])
  current[mapply(is.infinite, current)] <- NA
  current <- current[,c(-1,-2,-12)]
  try(current <- subset(current, current$wateryear>1980 & current$wateryear<2021))
  # change this based on trend period ie 1950 to 2018, 1950 - 2018
  years <- as.data.frame(1981:2020)
  nathresh <- 4 # less than 10% missing years
  # rest does not need changing
  colnames(years) <- "wateryear"
  current <- merge(current,years, by = "wateryear", all = TRUE)
  results <- data.frame(matrix(NA, nrow = 7, ncol = 7))
  rownames(results) <- colnames(current)[c(3:9)]
  colnames(results) <- c("gage","tau","pval","slope","total_change","season","suitable_length")
  
  results_winter <- results
  results_spring <- results
  results_summer <- results
  results_fall <- results
  
  current_winter <- subset(current, current$season == "winter")
  current_spring <- subset(current, current$season == "spring")
  current_summer <- subset(current, current$season == "summer")
  current_fall <- subset(current, current$season == "fall")
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_winter)[c+2]
    currentcolumn <- as.data.frame(current_winter[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_winter$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"winter",suitable_trend_length)
      results_winter[(c),] <- output
    } 
  }
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_spring)[c+2]
    currentcolumn <- as.data.frame(current_spring[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_spring$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"spring",suitable_trend_length)
      results_spring[(c),] <- output
    } 
  }
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_summer)[c+2]
    currentcolumn <- as.data.frame(current_summer[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_summer$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"summer",suitable_trend_length)
      results_summer[(c),] <- output
    } 
  }
  
  for(c in 1:7){
    # c = 1
    currentcolumnname <- colnames(current_fall)[c+2]
    currentcolumn <- as.data.frame(current_fall[,c+2])
    colnames(currentcolumn) <- "variable"
    currentcolumn$wyear <- current_fall$wateryear
    currentcolumn <- merge(currentcolumn,years, by.x = "wyear", by.y = "wateryear", all = TRUE)
    nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
    suitable_trend_length <- ifelse(nacounts < nathresh, "yes","no")
    
    if(nacounts < nathresh){
      manken <- MannKendall(currentcolumn$variable)
      sen <- zyp.sen(variable~wyear, currentcolumn)
      tau <- manken$tau
      pval <- manken$sl
      total_change <- sen$coefficients[2] * period_length ### Sen slope of trend (per year multiplied by number of years)
      output <- c(swsites[i], tau,pval,sen$coefficients[2],total_change,"fall",suitable_trend_length)
      results_fall[(c),] <- output
    } 
  }
  
  results <- rbind(results_winter,results_spring, results_summer, results_fall)
  write.csv(results, paste(swsites[i],".trend.summary.csv",sep = ""))}

####################################################################################
####################################################################################
####################################################################################
####################################################################################
# merge individual site trend files
setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19212020_seasonal_NATIONAL")
all1921 <- list.files(pattern = ".csv")
alldata1921 <- do.call(rbind, lapply(all1921, read.csv))

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19512020_seasonal_NATIONAL")
all1951 <- list.files(pattern = ".csv")
alldata1951 <- do.call(rbind, lapply(all1951, read.csv))

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\trends_19812020_seasonal_NATIONAL")
all1981 <- list.files(pattern = ".csv")
alldata1981 <- do.call(rbind, lapply(all1981, read.csv))


setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\master_files_NATIONAL")

alldata1921$X <- str_replace(alldata1921$X, "1","")
alldata1921$X <- str_replace(alldata1921$X, "2","")
alldata1921$X <- str_replace(alldata1921$X, "3","")

alldata1951$X <- str_replace(alldata1951$X, "1","")
alldata1951$X <- str_replace(alldata1951$X, "2","")
alldata1951$X <- str_replace(alldata1951$X, "3","")

alldata1981$X <- str_replace(alldata1981$X, "1","")
alldata1981$X <- str_replace(alldata1981$X, "2","")
alldata1981$X <- str_replace(alldata1981$X, "3","")

write.csv(alldata1921, "trends_in_19212020_seasonal_streamflow_metrics_NATIONAL_04212023.csv")
write.csv(alldata1951, "trends_in_19512020_seasonal_streamflow_metrics_NATIONAL_04212023.csv")
write.csv(alldata1981, "trends_in_19812020_seasonal_streamflow_metrics_NATIONAL_04212023.csv")

