ttlight <- function(mydata_49, plot_label) {



  mydata_49$gain_factor[mydata_49$gain == 0] <- 64/1
  mydata_49$gain_factor[mydata_49$gain == 1] <- 64/3.7
  mydata_49$gain_factor[mydata_49$gain == 2] <- 64/16
  mydata_49$gain_factor[mydata_49$gain == 3] <- 64/64

  mydata_49$integration_factor <- mydata_49$integration_T/50

  mydata_49$AS7263_610 <- as.numeric(mydata_49$AS7263_610); mydata_49$AS7263_610[mydata_49$AS7263_610 > 65000] <- NA
  mydata_49$AS7263_680 <- as.numeric(mydata_49$AS7263_680); mydata_49$AS7263_680[mydata_49$AS7263_680 > 65000] <- NA
  mydata_49$AS7263_730 <- as.numeric(mydata_49$AS7263_730); mydata_49$AS7263_730[mydata_49$AS7263_730 > 65000] <- NA
  mydata_49$AS7263_760 <- as.numeric(mydata_49$AS7263_760); mydata_49$AS7263_760[mydata_49$AS7263_760 > 65000] <- NA
  mydata_49$AS7263_810 <- as.numeric(mydata_49$AS7263_810); mydata_49$AS7263_810[mydata_49$AS7263_810 > 65000] <- NA
  mydata_49$AS7263_860 <- as.numeric(mydata_49$AS7263_860); mydata_49$AS7263_860[mydata_49$AS7263_860 > 65000] <- NA
  mydata_49$AS7262_450 <- as.numeric(mydata_49$AS7262_450); mydata_49$AS7262_450[mydata_49$AS7262_450 > 65000] <- NA
  mydata_49$AS7262_500 <- as.numeric(mydata_49$AS7262_500); mydata_49$AS7262_500[mydata_49$AS7262_500 > 65000] <- NA
  mydata_49$AS7262_550 <- as.numeric(mydata_49$AS7262_550); mydata_49$AS7262_550[mydata_49$AS7262_550 > 65000] <- NA
  mydata_49$AS7262_570 <- as.numeric(mydata_49$AS7262_570); mydata_49$AS7262_570[mydata_49$AS7262_570 > 65000] <- NA
  mydata_49$AS7262_600 <- as.numeric(mydata_49$AS7262_600); mydata_49$AS7262_600[mydata_49$AS7262_600 > 65000] <- NA
  mydata_49$AS7262_650 <- as.numeric(mydata_49$AS7262_650); mydata_49$AS7262_650[mydata_49$AS7262_650 > 65000] <- NA

  #Near Infrared
  AS7263_610_R <-
   -312.45+(1.6699* (mydata_49$AS7263_610*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7263_680_R <-
   -561.56+(1.5199* (mydata_49$AS7263_680*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7263_730_R <-
   -1511.2+(1.6209* (mydata_49$AS7263_730*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7263_760_R <-
   -1012.5+(1.4549* (mydata_49$AS7263_760*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7263_810_R <-
   91.58+(0.8414* (mydata_49$AS7263_810*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7263_860_R <-
   334.88+(0.531* (mydata_49$AS7263_860*mydata_49$integration_factor*mydata_49$gain_factor))

  #Visible Light Spectrum
  AS7262_450_R <-
   -212.62+(0.4562* (mydata_49$AS7262_450*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7262_500_R <-
   -232.13+(0.6257 * (mydata_49$AS7262_500*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7262_550_R <-
   -842.1+(1.0546 * (mydata_49$AS7262_550*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7262_570_R <-
   -666.72+(1.0462 * (mydata_49$AS7262_570*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7262_600_R <-
   -328.08+(0.8654 * (mydata_49$AS7262_600*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7262_650_R <-
   202.77+(0.7829* (mydata_49$AS7262_650*mydata_49$integration_factor*mydata_49$gain_factor))

  TT_ID <- mydata_49$IT_ID

  out <- data.frame(TT_ID, AS7262_450_R, AS7262_500_R, AS7262_550_R, AS7262_570_R, AS7262_600_R, AS7262_650_R, AS7263_610_R, AS7263_680_R, AS7263_730_R, AS7263_760_R, AS7263_810_R, AS7263_860_R)
  out[out<0] <- NA

  write.table(x = out, file = "ttlight_test.csv", row.names = F)

#plotting

par(mfrow=c(1,1))
  ID <- unique(out$TT_ID)
  for (j in 1:length(ID)){
    for (col in 2:13){
      channel <- out[,col]
      ts <- channel[out$TT_ID == ID[j]]
     if (length(ts) < 11) {
        next()
      }
    #ts_filt <- savitzkyGolay(ts, 0, 1, 11)
    out[col, out$IT_ID == ID[j]] <- ts[1:length(ts)]
    plot(ts, typ="l", ylim=c(0,10000))
    }
  }
