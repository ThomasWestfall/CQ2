# Re-formats raw WQ and streamflow data from .txt files to dataframe
# Only for Victoria water quality records
# Infills date and time if missing
# Does not infill or do anything with data

getRawDataHourly <- function(path_of_files, output_path, sensor_ID, TDS_factor){

  # if want to shop for ID...
  # sensor_ID <- list.files(path = path_of_files, full.names = FALSE, recursive = TRUE)
  #
  # #remove last 4 characters, ".txt"
  # sensor_ID <- substr(sensor_ID,1,nchar(sensor_ID)-4)

  #option to create output folder
  folders <- list.dirs(path = paste(output_path), full.names = FALSE, recursive = TRUE)

  setwd(paste(output_path,"/",sensor_ID,sep=""))
  getwd()

  filename <- paste(path_of_files,"/",sensor_ID,".txt", sep = "")

  # Read tabular data into R
  sensor <- read.table(filename, header = FALSE, sep =",", fill = TRUE)

  #delete second row or delete all and rename via indx?
  sensor <- sensor[-c(1,2,3),]

  #rename columns
  colnames(sensor) <- c('TimeDate','FLOW_hrly','FLOW_hrly_e','TURB_SENS_NTU','TURB_SENS_NTU_e','EC_SENS_uscm','EC_SENS_uscm_e','DO_SENS_ppm','DO_SENS_ppm_e','pH_SENS','pH_SENS_e','TEMP_SENS_C','TEMP_SENS_C_e')

  #NA in timeDate
  before <- sum(sapply(sensor$TimeDate, function(x) sum(is.na(x))))

  ##########reformat datetime
  sensor$TimeDate <- strptime(sensor$TimeDate,format="%H:%M:%S %d/%m/%Y",tz = "Etc/GMT-10" ) #or Etc/GMT-10, whatever timezone it was monitored in!
  sensor$TimeDate <- format(sensor$TimeDate, "%Y-%m-%d %H:%M:%S")
  sensor$TimeDate <- strptime(sensor$TimeDate, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-10" )

  #NA in timeDate after timeDate format
  after <- sum(sapply(sensor$TimeDate, function(x) sum(is.na(x))))

  print(paste("before ",before,sep=""))
  print(paste("after ",after,sep=""))

  if(after != before){stop('after =/ before')}

  #all columns numeric
  cols.num <- c('FLOW_hrly','FLOW_hrly_e','TURB_SENS_NTU','TURB_SENS_NTU_e','EC_SENS_uscm','EC_SENS_uscm_e','DO_SENS_ppm','DO_SENS_ppm_e','pH_SENS','pH_SENS_e','TEMP_SENS_C','TEMP_SENS_C_e')
  sensor[cols.num] <- sapply(sensor[cols.num],as.numeric)

  #add TDS column
  sensor$TDS <- sensor$EC_SENS_uscm*TDS_factor#0.6 from DEWLP (Natalie paper)

  #WQ columns
  WQ_col <- c('TURB_SENS_NTU','EC_SENS_uscm','DO_SENS_ppm','pH_SENS','TEMP_SENS_C','TDS')

  #### create dataframe for every hour in record IF required (for irregular timeseries)
  start <- as_datetime(sensor[1,1])
  end <- as_datetime(sensor[nrow(sensor),1])
  diff_dates <- difftime(end,start, units = "hours")

  #only infills date and time, discharge and WQ values left alone
  if(diff_dates > nrow(sensor)){
    sensor_all <- data.frame(matrix(ncol = ncol(sensor), nrow = (diff_dates)))
    colnames(sensor_all) <- colnames(sensor)
    sensor_all$TimeDate <- seq(from=start, to=end-1, by = "hours")
    sensor_all <- rbind(sensor,sensor_all)
    sensor_all <- sensor_all %>% arrange(TimeDate) %>% filter(!duplicated(TimeDate))
    sensor_all <-head(sensor_all, -1)
    sensor <- sensor_all
  }

  sensor_hrly <- sensor


  return(sensor_hrly)
}
