ttscrape <- function(ID, subset_days) {

  library(tidyr)
  library(ggplot2)
  library(tidyverse)
  library(signal)
  library(zoo)
  library(prospectr)

  #Specifying the url for desired website to be scraped
  url <-
    paste("http://naturetalkers.altervista.org/",
          ID,
          "/ttcloud.txt",
          sep = "")


  if (RCurl::url.exists(url)==T){
    #Reading the HTML code from the website
    mydata0 <- read.csv(url,
                        sep = ";",
                        header = FALSE,
                        fill = TRUE)
    flag0 <- 1
  } else {flag0 <- 0}

  #Specifying the url for desired website to be scraped
  url <-
    paste("http://ittn.altervista.org/", ID, "/ttcloud.txt", sep = "")
  #Reading the HTML code from the website


  if (RCurl::url.exists(url)==T){
    mydata1 <- read.csv(url,
                        sep = ";",
                        header = FALSE,
                        fill = TRUE)
    flag1 <- 1
  } else {flag1 <- 0}


  if (flag0 == 0){
    if (flag1 == 0){stop()}else{mydata <- mydata1}
  }else{
    if (flag1 == 0){mydata <- mydata0}
    else{
      #for some sites the number of variables from the old to the new serve changes
      varnum_mydata0 <- dim(mydata0)[2]
      varnum_mydata1 <- dim(mydata1)[2]
      varnum <- min(varnum_mydata0, varnum_mydata1)
      mydata0 <- mydata0[,1:varnum]
      mydata1 <- mydata1[,1:varnum,]
      mydata <- rbind(mydata0, mydata1)
      }
  }


  mydata_sep <-
    separate(mydata,
             V1,
             into = c("Date", "Time", "SN"),
             sep = "[ ,]")

  #split the dataset
  mydata_4D <- mydata_sep[mydata_sep$V3 == "4D", ]
  mydata_4D <- mydata_4D[mydata_4D$SN>52000000,]#TreeTalkers v3.2 have and ID higher than 52000000

  mydata_49 <- mydata_sep[mydata_sep$V3 == "49", ]
  mydata_49 <- mydata_49[mydata_49$SN>52000000,]#TreeTalkers v3.2 have and ID higher than 52000000

  mydata_4B <- mydata_sep[mydata_sep$V3 == "4B", ]#the string 4B and 4C contain only TTcloud data

  mydata_4C <- mydata_sep[mydata_sep$V3 == "4C", ]#the string 4B and 4C contain only TTcloud data


  header_4D <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "Tref_0",
      "Theat_0",
      "growt_sens",
      "adc_bandgap",
      "Bits",
      "RH",
      "Tair",
      "gz_mean",
      "gz_sd",
      "gy_mean",
      "gy_sd",
      "gx_mean",
      "gx_sd",
      "Tref_1",
      "Theat_1",
      "StWC",
      "adc_Vbat"
    )
  header_49 <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "AS7263_610",
      "AS7263_680",
      "AS7263_730",
      "AS7263_760",
      "AS7263_810",
      "AS7263_860",
      "AS7262_450",
      "AS7262_500",
      "AS7262_550",
      "AS7262_570",
      "AS7262_600",
      "AS7262_650",
      "integration_T",
      "gain"
    )

  header_4B <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "Acc_Recs",
      "Recs_to_be_sent",
      "MCC_tel_op",
      "MNC_tel_op",
      "GSM_reg",
      "GSM_field",
      "Battery",
      "Firmware_ver"
    )

  header_4C <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "TBL_locked",
      "n_first_sens",
      "RSSI_TT1",
      "RSSI_TT2",
      "RSSI_TT3",
      "RSSI_TT4",
      "RSSI_TT5",
      "RSSI_TT6",
      "RSSI_TT7",
      "RSSI_TT8",
      "RSSI_TT9",
      "RSSI_TT10",
      "RSSI_TT11",
      "RSSI_TT12",
      "RSSI_TT13",
      "RSSI_TT14",
      "RSSI_TT15",
      "RSSI_TT16",
      "RSSI_TT17",
      "RSSI_TT18",
      "RSSI_TT19",
      "RSSI_TT20"
    )

  colnames(mydata_4D) <- header_4D

  colnames(mydata_49) <- header_49

  colnames(mydata_4B) <- header_4B

  header_4C <- header_4C[1:varnum]
  colnames(mydata_4C) <- header_4C

  #filter all the dates earlier than 2020-01-01 00:00:00
  mydata_4D$Timestamp[mydata_4D$Timestamp < 1577836800] <- NA

  #convert the ids to integers
  mydata_4D$TT_ID <- as.integer(mydata_4D$TT_ID)
  mydata_49$TT_ID <- as.integer(mydata_49$TT_ID)
  mydata_4B$TT_ID <- as.integer(mydata_4B$TT_ID);
  mydata_4C$TT_ID <- as.integer(mydata_4C$TT_ID);


  mydata_4D$gz_mean <- as.integer(mydata_4D$gz_mean)


  #timestamp conversion
  #Timestamp of the router when the individual device send the data string. The timestamp is the number of seconds since 1st January 1970.
  mydata_4D$Timestamp <-
    as.POSIXct(mydata_4D$Timestamp, origin = "1970-01-01", tz="GMT")
  mydata_4B$Timestamp <-
    as.POSIXct(mydata_4B$Timestamp, origin = "1970-01-01", tz="GMT")
  mydata_49$Timestamp <-
    as.POSIXct(mydata_49$Timestamp, origin = "1970-01-01", tz="GMT")
  #create a color index
  id_col <- mydata_4D$TT_ID
  id_col[id_col == max(id_col, na.rm = T)] <- 21
  id_col[id_col != 21] <- id_col[id_col != 21] - max(id_col, na.rm = T)
  mydata_4D$id_col <- abs(id_col)


  if (subset_days != "all"){
  tt_begin <- mydata_4B$Timestamp[length(mydata_4B$Timestamp)] - (24*60*60*subset_days)
  tt_end <- mydata_4B$Timestamp[length(mydata_4B$Timestamp)]
  mydata_4B <- mydata_4B[mydata_4B$Timestamp > tt_begin,]
  mydata_4B <<- mydata_4B[is.na(mydata_4B$Timestamp) == FALSE,] #remove NAs
  } else {mydata_4B <<- mydata_4B}

  if (subset_days != "all"){
    tt_begin <- mydata_4D$Timestamp[length(mydata_4D$Timestamp)] - (24*60*60*subset_days)
    tt_end <- mydata_4D$Timestamp[length(mydata_4D$Timestamp)]
    mydata_4D <- mydata_4D[mydata_4D$Timestamp > tt_begin,]
    mydata_4D <<- mydata_4D[is.na(mydata_4D$Timestamp) == FALSE,] #remove NAs
  } else {mydata_4D <<- mydata_4D}

  if (subset_days != "all"){
    tt_begin <- mydata_49$Timestamp[length(mydata_49$Timestamp)] - (24*60*60*subset_days)
    tt_end <- mydata_49$Timestamp[length(mydata_49$Timestamp)]
    mydata_49 <- mydata_49[mydata_49$Timestamp > tt_begin,]
    mydata_49 <<- mydata_49[is.na(mydata_49$Timestamp) == FALSE,] #remove NAs
  } else {mydata_49 <<- mydata_49}


}
