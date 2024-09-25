#' @export

ttScrape_v2 <- function (ID, subset_days)
  {
    if (missing(subset_days)) {
      subset_days = "all"
    }
    library(tidyr)
    library(ggplot2)
    library(tidyverse)
    library(signal)
    library(zoo)
    library(lubridate)
    library(prospectr)

    options(timeout=800)

    url <- paste("http://naturetalkers.altervista.org/", ID,
                 "/ttcloud.txt", sep = "")
    if (RCurl::url.exists(url) == T) {
      mydata0 <- data.table::fread(url, sep = ";", header = FALSE,
                                   fill = TRUE, integer64 = "numeric")
      flag0 <- 1
    }
    else {
      flag0 <- 0
    }
    url <- paste("http://ittn.altervista.org/", ID, "/ttcloud.txt",
                 sep = "")
    if (RCurl::url.exists(url) == T) {
      mydata1 <- data.table::fread(url, sep = ";", header = FALSE,
                                   fill = TRUE, integer64 = "numeric")
      flag1 <- 1
    }
    else {
      flag1 <- 0
    }
    if (flag0 == 0) {
      if (flag1 == 0) {
        stop()
      }
      else {
        mydata <- mydata1
      }
    }
    else {
      if (flag1 == 0) {
        mydata <- mydata0
      }
      else {
        varnum_mydata0 <- dim(mydata0)[2]
        varnum_mydata1 <- dim(mydata1)[2]
        varnum <- min(varnum_mydata0, varnum_mydata1)
        mydata0 <- mydata0[, 1:varnum]
        mydata1 <- mydata1[, 1:varnum, ]
        mydata <- rbind(mydata0, mydata1)
      }
    }
    mydata <- mydata[mydata$V1 != 0, ]
    mydata_sep <- separate(mydata, V1, into = c("Date", "Time",
                                                "SN"), sep = "[ ,]")
    mydata_4D <- mydata_sep[mydata_sep$V3 == "4D", ]
    mydata_4D$SN <- as.numeric(mydata_4D$SN)
    mydata_4D <- mydata_4D[mydata_4D$SN > 5.2e+07, ]
    mydata_4D <- Filter(function(x) !all(is.na(x)), mydata_4D)
    mydata_4D <- mydata_4D %>% mutate_if(bit64::is.integer64,
                                         as.numeric)
    mydata_49 <- mydata_sep[mydata_sep$V3 == "49", ]
    mydata_49$SN <- as.numeric(mydata_49$SN)
    mydata_49 <- mydata_49[mydata_49$SN > 5.2e+07, ]
    mydata_49 <- Filter(function(x) !all(is.na(x)), mydata_49)

    mydata_49$V5 <- as.numeric(mydata_49$V5)
    mydata_49$V6 <- as.numeric(mydata_49$V6)
    mydata_49$V7 <- as.numeric(mydata_49$V7)
    mydata_49$V8 <- as.numeric(mydata_49$V8)
    mydata_49$V9 <- as.numeric(mydata_49$V9)
    mydata_49$V10 <- as.numeric(mydata_49$V10)
    mydata_49$V11 <- as.numeric(mydata_49$V11)
    mydata_49$V12 <- as.numeric(mydata_49$V12)
    mydata_49$V13 <- as.numeric(mydata_49$V13)
    mydata_49$V14 <- as.numeric(mydata_49$V14)
    mydata_49$V15 <- as.numeric(mydata_49$V15)
    mydata_49$V16 <- as.numeric(mydata_49$V16)

    mydata_4B <- mydata_sep[mydata_sep$V3 == "4B", ]
    mydata_4B <- Filter(function(x) !all(is.na(x)), mydata_4B)
    mydata_4C <- mydata_sep[mydata_sep$V3 == "4C", ]
    mydata_4C <- Filter(function(x) !all(is.na(x)), mydata_4C)
    header_4D <- c("SDate", "STime", "TT_ID", "Rec_Nr", "Dev_Ty",
                   "Timestamp", "Tref_0", "Theat_0", "growt_sens", "adc_bandgap",
                   "Bits", "RH", "Tair", "gz_mean", "gz_sd", "gy_mean",
                   "gy_sd", "gx_mean", "gx_sd", "Tref_1", "Theat_1", "StWC",
                   "adc_Vbat")
    header_49 <- c("SDate", "STime", "TT_ID", "Rec_Nr", "Dev_Ty",
                   "Timestamp", "AS7263_610", "AS7263_680", "AS7263_730",
                   "AS7263_760", "AS7263_810", "AS7263_860", "AS7262_450",
                   "AS7262_500", "AS7262_550", "AS7262_570", "AS7262_600",
                   "AS7262_650", "integration_T", "gain")
    header_4B <- c("SDate", "STime", "TT_ID", "Rec_Nr", "Dev_Ty",
                   "Timestamp", "Acc_Recs", "Recs_to_be_sent", "MCC_tel_op",
                   "MNC_tel_op", "GSM_reg", "GSM_field", "Battery", "Firmware_ver")
    header_4C <- c("SDate", "STime", "TT_ID", "Rec_Nr", "Dev_Ty",
                   "Timestamp", "TBL_locked", "n_first_sens", "RSSI_TT1",
                   "RSSI_TT2", "RSSI_TT3", "RSSI_TT4", "RSSI_TT5", "RSSI_TT6",
                   "RSSI_TT7", "RSSI_TT8", "RSSI_TT9", "RSSI_TT10", "RSSI_TT11",
                   "RSSI_TT12", "RSSI_TT13", "RSSI_TT14", "RSSI_TT15", "RSSI_TT16",
                   "RSSI_TT17", "RSSI_TT18", "RSSI_TT19", "RSSI_TT20", "RSSI_TT21",
                   "RSSI_TT22", "RSSI_TT23", "RSSI_TT24")
    colnames(mydata_4D) <- header_4D
    colnames(mydata_49) <- header_49
    colnames(mydata_4B) <- header_4B
    header_4C <- header_4C[1:(dim(mydata_4C)[2])]
    colnames(mydata_4C) <- header_4C

    #--------------
    mydata_4D$Timestamp <- as.double(mydata_4D$Timestamp)
    mydata_4D$Timestamp[mydata_4D$Timestamp < 1577836800] <- NA
    mydata_4B$Timestamp <- as.double(mydata_4B$Timestamp)
    mydata_4B$Timestamp[mydata_4B$Timestamp < 1577836800] <- NA
    mydata_49$Timestamp <- as.double(mydata_49$Timestamp)
    mydata_49$Timestamp[mydata_49$Timestamp < 1577836800] <- NA
    mydata_4C$Timestamp <- as.double(mydata_4C$Timestamp)
    mydata_4C$Timestamp[mydata_4C$Timestamp < 1577836800] <- NA
    #--------------

    mydata_4D$TT_ID <- as.integer(mydata_4D$TT_ID)
    mydata_49$TT_ID <- as.integer(mydata_49$TT_ID)
    mydata_4B$TT_ID <- substr(mydata_4B$TT_ID, 2, 8)
    mydata_4B$TT_ID <- as.integer(mydata_4B$TT_ID)
    mydata_4B$Acc_Recs <- as.integer(mydata_4B$Acc_Recs)
    mydata_4B$MCC_tel_op <- as.integer(mydata_4B$MCC_tel_op)
    mydata_4B$MNC_tel_op <- as.integer(mydata_4B$MCC_tel_op)
    mydata_4B$GSM_reg <- as.integer(mydata_4B$GSM_reg)
    mydata_4B$GSM_field <- as.integer(mydata_4B$GSM_field)
    mydata_4B$Battery <- as.integer(mydata_4B$Battery)
    mydata_4C$TT_ID <- substr(mydata_4C$TT_ID, 2, 8)
    mydata_4C$TT_ID <- as.integer(mydata_4C$TT_ID)
    mydata_4D$gz_mean <- as.integer(mydata_4D$gz_mean)

    #--------------
    mydata_4D$Timestamp <- as.POSIXct(mydata_4D$Timestamp, origin = "1970-01-01", tz = "GMT")
    mydata_4B$Timestamp <- as.POSIXct(mydata_4B$Timestamp, origin = "1970-01-01", tz = "GMT")
    mydata_49$Timestamp <- as.POSIXct(mydata_49$Timestamp, origin = "1970-01-01", tz = "GMT")
    mydata_4C$Timestamp <- as.POSIXct(mydata_4C$Timestamp, origin = "1970-01-01", tz = "GMT")
    #--------------

    #--------------
    mydata_4D$Timestamp <- floor_date(mydata_4D$Timestamp, "hour") #  FLOORING DATE TO THE HOUR, AS SOMETIMES THE MINUTE IS NOT 00 (12:00:01 will become 12:00:00)
    mydata_4B$Timestamp <- floor_date(mydata_4B$Timestamp, "hour") #  FLOORING DATE TO THE HOUR, AS SOMETIMES THE MINUTE IS NOT 00 (12:00:01 will become 12:00:00)
    mydata_49$Timestamp <- floor_date(mydata_49$Timestamp, "hour") #  FLOORING DATE TO THE HOUR, AS SOMETIMES THE MINUTE IS NOT 00 (12:00:01 will become 12:00:00)
    mydata_4C$Timestamp <- floor_date(mydata_4C$Timestamp, "hour") #  FLOORING DATE TO THE HOUR, AS SOMETIMES THE MINUTE IS NOT 00 (12:00:01 will become 12:00:00)
    #--------------


    if (subset_days != "all") {
      tt_begin <- mydata_4B$Timestamp[length(mydata_4B$Timestamp)] -
        (24 * 60 * 60 * subset_days)
      mydata_4B <- mydata_4B[mydata_4B$Timestamp > tt_begin,
      ]
      mydata_4B <- mydata_4B[is.na(mydata_4B$Timestamp) ==
                               FALSE, ]
    }
    else {
      mydata_4B <- mydata_4B
    }
    mydata_4B <- mydata_4B %>% drop_na(Timestamp, TT_ID)
    mydata_4B <- mydata_4B %>% distinct(TT_ID, Timestamp, .keep_all = TRUE)
    .GlobalEnv$mydata_4B <- mydata_4B
    if (subset_days != "all") {
      tt_begin <- mydata_4D$Timestamp[length(mydata_4D$Timestamp)] -
        (24 * 60 * 60 * subset_days)
      mydata_4D <- mydata_4D[mydata_4D$Timestamp > tt_begin,
      ]
      mydata_4D <- mydata_4D[is.na(mydata_4D$Timestamp) ==
                               FALSE, ]
    }
    else {
      mydata_4D <- mydata_4D
    }
    mydata_4D <- mydata_4D %>% drop_na(Timestamp, TT_ID)
    mydata_4D <- mydata_4D %>% distinct(TT_ID, Timestamp, .keep_all = TRUE)
    if (subset_days != "all") {
      tt_begin <- mydata_49$Timestamp[length(mydata_49$Timestamp)] -
        (24 * 60 * 60 * subset_days)
      mydata_49 <- mydata_49[mydata_49$Timestamp > tt_begin,
      ]
      mydata_49 <- mydata_49[is.na(mydata_49$Timestamp) ==
                               FALSE, ]
    }
    else {
      mydata_49 <- mydata_49
    }
    mydata_49 <- mydata_49 %>% drop_na(Timestamp, TT_ID)
    mydata_49 <- mydata_49 %>% distinct(TT_ID, Timestamp, .keep_all = TRUE)
    if (subset_days != "all") {
      tt_begin <- mydata_4C$Timestamp[length(mydata_4C$Timestamp)] -
        (24 * 60 * 60 * subset_days)
      mydata_4C <- mydata_4C[mydata_4C$Timestamp > tt_begin,
      ]
      mydata_4C <- mydata_4C[is.na(mydata_4C$Timestamp) ==
                               FALSE, ]
    }
    else {
      mydata_4C <- mydata_4C
    }
    mydata_4C <- mydata_4C %>% drop_na(Timestamp, TT_ID)
    mydata_4C <- mydata_4C %>% distinct(Timestamp, .keep_all = TRUE)
    .GlobalEnv$mydata_4C <- mydata_4C

    #
    #
    # Assuming unique TT_IDs can be gatheres by looking at all TT_IDs in the table, as every TTalker will send at least one signal during the monitoring period
    #
    #
    #

    unique_TT_IDs <- unique(mydata_4D$TT_ID) # Replace with your actual TT_IDs


    # Function to ensure all TT_IDs are present for each timestamp
    complete_TT_IDs <- function(data) {
      # Find the minimum and maximum timestamps in the data
      min_timestamp <- min(data$Timestamp, na.rm = TRUE)
      max_timestamp <- max(data$Timestamp, na.rm = TRUE)

      # Generate a sequence of timestamps from the minimum to maximum, spaced by hour
      full_timestamps <- seq(from = min_timestamp, to = max_timestamp, by = "hour")

      # Create a complete grid of all possible Timestamps and TT_IDs
      complete_grid <- expand.grid(Timestamp = full_timestamps,
                                   TT_ID = unique_TT_IDs)

      # Left join with the original data to ensure all combinations are present
      complete_data <- left_join(complete_grid, data, by = c("Timestamp", "TT_ID"))

      return(complete_data)
    }

    # Apply the function to each of your data subsets
    mydata_4D <- complete_TT_IDs(mydata_4D)
    mydata_49 <- complete_TT_IDs(mydata_49)


    mydata_4D$AcDate <- as.POSIXct(str_sub(mydata_4D$Timestamp, 1, 10), format="%Y-%m-%d", tz="GMT")
    mydata_4D$AcTime <- strftime(as.POSIXct(str_sub(mydata_4D$Timestamp, 12, 19), format="%H:%M:%OS"), format="%H")
    mydata_4C$AcDate <- as.POSIXct(str_sub(mydata_4C$Timestamp, 1, 10), format="%Y-%m-%d", tz="GMT")
    mydata_4C$AcTime <- strftime(as.POSIXct(str_sub(mydata_4C$Timestamp, 12, 19), format="%H:%M:%OS"), format="%H")
    mydata_49$AcDate <- as.POSIXct(str_sub(mydata_49$Timestamp, 1, 10), format="%Y-%m-%d", tz="GMT")
    mydata_49$AcTime <- strftime(as.POSIXct(str_sub(mydata_49$Timestamp, 12, 19), format="%H:%M:%OS"), format="%H")
    mydata_4B$AcDate <- as.POSIXct(str_sub(mydata_4B$Timestamp, 1, 10), format="%Y-%m-%d", tz="GMT")
    mydata_4B$AcTime <- strftime(as.POSIXct(str_sub(mydata_4B$Timestamp, 12, 19), format="%H:%M:%OS"), format="%H")

    .GlobalEnv$mydata_4D <- mydata_4D
    .GlobalEnv$mydata_49 <- mydata_49

}
