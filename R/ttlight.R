ttlight <- function(mydata_49, lat, lon){

  #example call(ttlight(mydata_49, lat=46.453, lon=11.232))

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

  specttRal <- data.frame(TT_ID,
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
  specttRal <- specttRal[format(specttRal$Timestamp, format="%Y")>2010,]

  #keep data with with sun in +/-30 degrees from solar noon
  specttRal <- specttRal[(specttRal$azimuth > -30) & (specttRal$azimuth < 30),]


  datalist = list() #initialize the list
  ID <- unique(specttRal$TT_ID)
  for (j in 1:length(ID)) {

    # Subset dataset for TT_IDs
    specttRal_L0 <- specttRal %>%
      dplyr::filter(TT_ID == ID[j])

    if (length(specttRal_L0$Timestamp)<10){next}

    #######aggregate to daily
    specttRal_L0$Day <- floor_date(specttRal_L0$Timestamp, "day")

    specttRal_L1 <- specttRal_L0 %>%
      group_by(Day)  %>%
      summarize(L450_R = median(AS7262_450_R, na.rm = TRUE),
                L500_R = median(AS7262_500_R, na.rm = TRUE),
                L550_R = median(AS7262_550_R, na.rm = TRUE),
                L570_R = median(AS7262_570_R, na.rm = TRUE),
                L600_R = median(AS7262_600_R, na.rm = TRUE),
                L650_R = median(AS7262_650_R, na.rm = TRUE),
                L610_R = median(AS7263_610_R, na.rm = TRUE),
                L680_R = median(AS7263_680_R, na.rm = TRUE),
                L730_R = median(AS7263_730_R, na.rm = TRUE),
                L760_R = median(AS7263_760_R, na.rm = TRUE),
                L810_R = median(AS7263_810_R, na.rm = TRUE),
                L860_R = median(AS7263_860_R, na.rm = TRUE)
      )
    specttRal_L1$TT_ID <- rep(ID[j], length(specttRal_L1$Day))
    #colnames(mydata_daily) <- c("date", "phi")
    datalist[[j]] <- specttRal_L1 # add it to your list

  }
  specttRal_L1_all <- dplyr::bind_rows(datalist)


  #create a color index
  id_col <- specttRal_L1_all$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col))); colnames(id_col_ind) <- c("TT_ID", "ID")
  #create index for color scale
  specttRal_L1_all$id_col_ind <- specttRal_L1_all$TT_ID
  for (i in 1:length(id_col_ind$ID)){
    specttRal_L1_all$id_col_ind <- replace(specttRal_L1_all$id_col_ind, specttRal_L1_all$id_col_ind==id_col_ind$TT_ID[i], id_col_ind$ID[i])
  }


  df1 <- data.frame(specttRal_L1_all$Day, specttRal_L1_all$L860_R, specttRal_L1_all$id_col_ind)
  colnames(df1) <- c("Timestamp", "L860_R", "id_col_ind")



  if (plot_label == "all_in_one"){
    df1$L860_R[df1$L860_R<50]<-NA
    p <- ggplot(data = df1, aes(Timestamp, L860_R)) +
      geom_point(aes(colour = id_col_ind), size = 0.2) +
      geom_smooth() +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "L860_R (counts/(µW/cm^2)") +
      #labs(title = site) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      ylim(quantile(df1$L860_R, p = 0.01, na.rm=T), quantile(df1$L860_R, p = 0.99, na.rm=T))
    print(p)
  }


  df1 <- data.frame(specttRal_L1_all$Day, specttRal_L1_all$L860_R, specttRal_L1_all$id_col_ind)
  colnames(df1) <- c("Timestamp", "L860_R", "id_col_ind")

  if (plot_label == "split"){
    p <- ggplot(data = df1, aes(Timestamp, L860_R, color = id_col_ind)) +
      geom_point(aes(group = "whatever"), size = 0.2) +
      #geom_line(aes(group = "whatever")) +
      facet_grid(facets = specttRal_L1_all$TT_ID ~ ., margins = FALSE) +
      geom_smooth() +
      labs(x = "Timestamp", y = "L860_R (counts/(µW/cm^2))") +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      ylim(quantile(df1$L860_R, p = 0.01, na.rm=T), quantile(df1$L860_R, p = 0.99, na.rm=T))
    print(p)
  }

  if (plot_label == "none"){}

  #create a data frame for output

  df_ttlight <<- subset(specttRal_L1_all, select=-id_col_ind)

}

