ttlight <- function(mydata_49, lat, lon){

  lat <- 46.453676
  lon <- 11.232666

  #load required packages
  library(suncalc)

  ID <- unique(mydata_49$IT_ID)

  #filter_ttlight <- function(mydata_49){
  ID <- unique(mydata_49$IT_ID)
  for (j in 1:length(ID)) {
    ts <- mydata_49$AS7263_610[mydata_49$IT_ID == ID[j]]
    if (length(ts) < 11) {
      next()
    }
    ts_filt <- savitzkyGolay(ts, 0, 1, 11)
    #mydata_49$AS7263_610[mydata_49$IT_ID == ID[j]] <- ts_filt[1:length(ts)]
  }


  #the gain correction seems to occur directly in the ams firmware
  mydata_49$gain_factor[mydata_49$gain == 0] <- 1
  mydata_49$gain_factor[mydata_49$gain == 1] <- 1
  mydata_49$gain_factor[mydata_49$gain == 2] <- 1
  mydata_49$gain_factor[mydata_49$gain == 3] <- 1

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
  AS7263_610_R <- mydata_49$AS7263_610*mydata_49$integration_factor*mydata_49$gain_factor
   #-312.45+(1.6699* (mydata_49$AS7263_610*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7263_680_R <- mydata_49$AS7263_680*mydata_49$integration_factor*mydata_49$gain_factor
   #-561.56+(1.5199* (mydata_49$AS7263_680*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7263_730_R <- mydata_49$AS7263_730*mydata_49$integration_factor*mydata_49$gain_factor
   #-1511.2+(1.6209* (mydata_49$AS7263_730*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7263_760_R <- mydata_49$AS7263_760*mydata_49$integration_factor*mydata_49$gain_factor
   #-1012.5+(1.4549* (mydata_49$AS7263_760*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7263_810_R <- mydata_49$AS7263_760*mydata_49$integration_factor*mydata_49$gain_factor
   #91.58+(0.8414* (mydata_49$AS7263_810*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7263_860_R <- mydata_49$AS7263_810*mydata_49$integration_factor*mydata_49$gain_factor
   #334.88+(0.531* (mydata_49$AS7263_860*mydata_49$integration_factor*mydata_49$gain_factor))

  #Visible Light Spectrum
  AS7262_450_R <- mydata_49$AS7262_450*mydata_49$integration_factor*mydata_49$gain_factor
   #-212.62+(0.4562* (mydata_49$AS7262_450*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7262_500_R <- mydata_49$AS7262_500*mydata_49$integration_factor*mydata_49$gain_factor
   #-232.13+(0.6257 * (mydata_49$AS7262_500*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7262_550_R <- mydata_49$AS7262_550*mydata_49$integration_factor*mydata_49$gain_factor
   #-842.1+(1.0546 * (mydata_49$AS7262_550*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7262_570_R <- mydata_49$AS7262_570*mydata_49$integration_factor*mydata_49$gain_factor
   #-666.72+(1.0462 * (mydata_49$AS7262_570*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7262_600_R <- mydata_49$AS7262_600*mydata_49$integration_factor*mydata_49$gain_factor
   #-328.08+(0.8654 * (mydata_49$AS7262_600*mydata_49$integration_factor*mydata_49$gain_factor))
  AS7262_650_R <- mydata_49$AS7262_650*mydata_49$integration_factor*mydata_49$gain_factor
   #202.77+(0.7829* (mydata_49$AS7262_650*mydata_49$integration_factor*mydata_49$gain_factor))

  #solar geometry
  #"altitude" : sun altitude above the horizon in radians, e.g. 0 at the horizon and PI/2 at the zenith (straight over your head)
  #"azimuth": sun azimuth in radians (direction along the horizon,measured from south to west), e.g. 0 is south and Math.PI * 3/4 is northwest

  solarGeom <-  getSunlightPosition(date= mydata_49$Timestamp, lat = lat, lon = lon)
  solarGeom$altitude <- (solarGeom$altitude * 180) / (pi)
  solarGeom$azimuth <-  (solarGeom$azimuth * 180) / (pi)

  TT_ID <- mydata_49$TT_ID
  Timestamp <- mydata_49$Timestamp

  out <- data.frame(TT_ID,
                    Timestamp,
                    subset(solarGeom, select =-date),
                    AS7262_450_R,
                    AS7262_500_R,
                    AS7262_550_R,
                    AS7262_570_R,
                    AS7262_600_R,
                    AS7262_650_R,
                    AS7263_610_R,
                    AS7263_680_R,
                    AS7263_730_R,
                    AS7263_760_R,
                    AS7263_810_R,
                    AS7263_860_R
                    )
  #out[out<0] <- NA

  #it is possible to find dates out of range
  out <- out[format(out$Timestamp, format="%Y")>2010,]

  #keep data with with sun in +/-20 degrees from solar noon
  out <- out[out$azimuth > -30 & out$azimuth < 30,]




#plotting
  #ID <- unique(mydata_4D$TT_ID)
  #plot(out$AS7263_610_R[out$TT_ID == ID[21]] ~ out$Timestamp[out$TT_ID == ID[21]], typ="l", col="red")
  #plot(out$AS7263_610_R[out$TT_ID == ID[1]] ~out$Timestamp[out$TT_ID == ID[1]], typ="l")


#par(mfrow=c(1,1))
#  ID <- unique(out$TT_ID)
#  for (j in 1:length(ID)){
#    for (col in 2:13){
#      channel <- out[,col]
#      ts <- channel[out$TT_ID == ID[j]]
#     if (length(ts) < 11) {
#        next()
#      }
#    #ts_filt <- savitzkyGolay(ts, 0, 1, 11)
#    out[col, out$IT_ID == ID[j]] <- ts[1:length(ts)]
#    plot(ts, typ="l", ylim=c(0,10000))
#    }
#  }
}

