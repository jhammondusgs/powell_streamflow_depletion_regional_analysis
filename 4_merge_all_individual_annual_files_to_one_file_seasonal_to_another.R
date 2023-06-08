# code below not updated to run for any user using github and google drive yet

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\streamflow_metrics_with_climate")
allannual <- list.files(pattern = "annual")
allannualdata <- do.call(rbind, lapply(allannual, read.csv))

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\streamflow_metrics")
allseasonal <- list.files(pattern = "seasonal")
allseasonaldata <- do.call(rbind, lapply(allseasonal, read.csv))

setwd("C:\\Users\\jhammond\\Desktop\\Powell_Streamflow_Depletion_September_2022\\Regional_analysis\\master_files")

write.csv(allannualdata, "water_year_streamflow_metrics_and_climate_data_10102022.csv")
write.csv(allseasonaldata, "seasonal_streamflow_metrics_10102022.csv")
