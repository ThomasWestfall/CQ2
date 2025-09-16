#' @export
getHourlyData <- function(sensor_hrly,catchment_area,important_parameter){

  #find first data row when most_important parameter data begins
  first_data_row <- which(!is.na(sensor_hrly[[important_parameter]]))[1]

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


  data_hrly <- data_hrly %>%
    group_by(TimeDate) %>%
    summarize(FLOW_hrly = FLOW_hrly, #ML/day
              FLOW_hrly_e = FLOW_hrly_e,
              FLOW_mm_hr = (((FLOW_hrly*1000)/catchment_area)/24)*1000, #ML/day to mm/hr
              EC_SENS_uscm = EC_SENS_uscm,
              EC_SENS_uscm_e = EC_SENS_uscm_e,
              TDS = TDS,
              TDS_load_kg_h = TDS*FLOW_hrly/24, #EC_load = FLOW_hrly*EC_SENS_uscm %>%
              TURB_SENS_NTU = TURB_SENS_NTU,
              TURB_SENS_NTU_e = TURB_SENS_NTU_e,
              DO_SENS_ppm = DO_SENS_ppm,
              pH_SENS = pH_SENS,
              pH_SENS_e = pH_SENS_e,
              TEMP_SENS_C = TEMP_SENS_C,
              TEMP_SENS_C_e= TEMP_SENS_C_e) %>%
    mutate(hour = lubridate::hour(TimeDate),day = lubridate::day(TimeDate),month = lubridate::month(TimeDate), year = lubridate::year(TimeDate),DecYear = lubridate::decimal_date(TimeDate))

  return(data_hrly)
}
