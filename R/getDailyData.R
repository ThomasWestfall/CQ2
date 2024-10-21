getDailyData <- function(sensor_hrly,catchment_area,important_parameter){

  #find first data row when most_important parameter data begins
  first_data_row <- 1 #which(!is.na(sensor_hrly[[important_parameter]]))[1]

  #filter, keep only rows after when most_important parameter data begins
  data_hrly <-  sensor_hrly[seq_along(sensor_hrly[,1]) >= first_data_row, ]

  # Find datetime values containing '00:00:00'
  df_midnight <- data_hrly[grepl("00:00:00", as.character(format(data_hrly$TimeDate,format="%Y-%m%-%d %T"))), ]

  #find first data row that begins at midnight
  first_midnight_row <- which(data_hrly$TimeDate == df_midnight[1,1])

  #begin at first midnight sample
  data_hrly <-  data_hrly[seq_along(data_hrly[,1]) >= first_midnight_row, ]

  #find last data row that begins at midnight
  last_midnight_row <- which(data_hrly$TimeDate == df_midnight[nrow(df_midnight),1])

  #end at last midnight sample, so only complete days in dataset
  data_hrly <- data_hrly[seq_along(data_hrly[,1]) < last_midnight_row, ]

  # High-frequency data has hourly time-step with Q units in ML/day, so taking mean of values...
  # BOM has data in 15 min intervals, so there is a slight discrepancy when compared with BOM daily data
  data_daily <- data_hrly %>%
    group_by(as.Date(TimeDate)) %>%
    summarize(FLOW_dly_sum = sum(FLOW_hrly), #this is sum of flow for entire day, ML/day, DO NOT USE
              FLOW_dly_mean = mean(FLOW_hrly), #this is average flow for entire day which is equal to the volume in ML, ML/day
              FLOW_dly_mean_e = mean(FLOW_hrly_e),
              FLOW_mm_d = ((FLOW_dly_mean*1000)/catchment_area)*1000, #ML/day to mm/day
              EC_SENS_uscm = sum(EC_SENS_uscm*FLOW_hrly)/sum(FLOW_hrly),
              EC_SENS_uscm_e = mean(EC_SENS_uscm_e),
              TDS = sum(TDS*FLOW_hrly)/sum(FLOW_hrly),
              # TDS_load_kg_d = sum(TDS_load_kg_h),
              TURB_SENS_NTU = mean(TURB_SENS_NTU),
              TURB_SENS_NTU_e = mean(TURB_SENS_NTU_e),
              DO_SENS_ppm = mean(DO_SENS_ppm),
              pH_SENS = mean(pH_SENS),
              pH_SENS_e = mean(pH_SENS_e),
              TEMP_SENS_C = mean(TEMP_SENS_C),
              TEMP_SENS_C_e= mean(TEMP_SENS_C_e))

  #rename TimeDate column
  colnames(data_daily)[1] = "Date"

  #add date columns
  data_daily <- data_daily %>%
    mutate(day = lubridate::day(Date),month = lubridate::month(Date), year = lubridate::year(Date),DecYear = lubridate::decimal_date(Date))

  return(data_daily)
}

