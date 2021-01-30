ttscrape <- function(ID) {
  #Specifying the url for desired website to be scraped
  url <-
    paste("http://naturetalkers.altervista.org/",
          ID,
          "/ttcloud.txt",
          sep = "")
  #Reading the HTML code from the website
  mydata0 <- read.csv(url,
                      sep = ";",
                      header = FALSE,
                      fill = TRUE)


  #Specifying the url for desired website to be scraped
  url <-
    paste("http://ittn.altervista.org/", ID, "/ttcloud.txt", sep = "")
  #Reading the HTML code from the website
  mydata1 <- read.csv(url,
                      sep = ";",
                      header = FALSE,
                      fill = TRUE)

  mydata <- rbind(mydata0, mydata1)


  mydata_sep <-
    separate(mydata,
             V1,
             into = c("Date", "Time", "SN"),
             sep = "[ ,]")

  #split the dataset
  mydata_4D <- mydata_sep[mydata_sep$V3 == "4D", ]

  mydata_49 <- mydata_sep[mydata_sep$V3 == "49", ]

  mydata_4B <- mydata_sep[mydata_sep$V3 == "4B", ]

  mydata_4C <- mydata_sep[mydata_sep$V3 == "4C", ]


  header_4D <-
    c(
      "SDate",
      "STime",
      "IT_ID",
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
      "IT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "AS7263_610",
      "AS7263_680",
      "AS7263_730",
      "AS7263_760",
      "AS7263_810",
      "AS7263_860",
      "AS7263_450",
      "AS7263_500",
      "AS7263_550",
      "AS7263_570",
      "AS7263_600",
      "AS7263_650",
      "integration_T",
      "gain"
    )

  header_4B <-
    c(
      "SDate",
      "STime",
      "IT_ID",
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
      "IT_ID",
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

  colnames(mydata_4C) <- header_4C

  mydata_4D$Timestamp[mydata_4D$Timestamp < 2000000] <- NA

  mydata_4D$IT_ID <- as.integer(mydata_4D$IT_ID)
  mydata_4D$gz_mean <- as.integer(mydata_4D$gz_mean)


  #timestamp conversion
  HR_Timestamp_4D <-
    as.POSIXct(mydata_4D$Timestamp, origin = "1970-01-01")
  HR_Timestamp_4B <-
    as.POSIXct(mydata_4B$Timestamp, origin = "1970-01-01")
  #create a color index
  id_col <- mydata_4D$IT_ID
  id_col[id_col == max(id_col, na.rm = T)] <- 21
  id_col[id_col != 21] <- id_col[id_col != 21] - max(id_col, na.rm = T)
  mydata_4D$id_col <- abs(id_col)

  mydata_4B <<- mydata_4B
  mydata_4D <<- mydata_4D

}
