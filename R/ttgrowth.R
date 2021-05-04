
ttgrowth <- function(mydata_4D) {

  library(treenetproc)
  #A 2nd-degree polynomial regression model applied to analyze the relationship between distance and reported digital numbers by a sharp sensor.

  x <- mydata_4D$growt_sens
  #conversion range according to the manual (September 2020)
  x[x > 85000] <- NA
  x[x < 40000] <- NA
  #f=y0+a*x+b*x^2+c*x^3
  a <- 0.000000008
  b <- -0.0016
  c <- 89.032
  f=a*x^2+b*x+c
  #a <- 237908.4541
  #b <- -1.1171
  #c <- 199.433

  #f=(a+b*x)/(c+x)

  #centimeters to micrometers
  f <- f*10000
  # to invert since growth equal less distance from trunk
  f <- 400000-f
  #remove baseline distance
  #ID <- unique(mydata_4D$IT_ID)
  #for (j in 1:length(ID)) {
  #  ts <- f[mydata_4D$IT_ID == ID[j]]
  #  if (length(ts) < 11) {
  #    next()
  #  }
  #  ts_filt <- ts-min(ts, na.rm=T)
  #  f[mydata_4D$IT_ID == ID[j]] <- ts_filt[1:length(ts)]
  #  #millimiters to micron
  #}



  ID <- unique(mydata_4D$IT_ID)
  for (j in 1:length(ID)) {
  myDendro_data_L0 <- data.frame(mydata_4D$IT_ID, mydata_4D$Timestamp, f)#as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01"), f)
  colnames(myDendro_data_L0) <- c("series", "ts", "value")

  # Subset dataset for example
  myDendro_data_L0 <- myDendro_data_L0 %>%
    dplyr::filter(series == ID[j])

  #use nighttime data (between midnight and 3am)
  myDendro_data_L0 <- myDendro_data_L0[as.POSIXlt(myDendro_data_L0$ts)$hour<3,]

  # align data
  myDendro_data_L1 <- proc_L1(data_L0 = myDendro_data_L0,
                            reso = 60,
                            input = "long",
                            date_format ="%Y-%m-%d %H:%M:%S",
                            year = "asis",
                            tz = "GMT")
  #myDendro_data_L1$value <-
  #  baytrends::fillMissing(myDendro_data_L1$value,
  #                         span = 12,
  #                         Dates = NULL,
  #                         max.fill = 12)#gapfillSSA(series = ts, plot.results = FALSE, open.plot = FALSE)



  myTemp_data_L0 <- data.frame(mydata_4D$IT_ID, as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01"), mydata_4D$Tair/10)
  colnames(myTemp_data_L0) <- c("series", "ts", "value")

  # Subset dataset for example
  myTemp_data_L0 <- myTemp_data_L0 %>%
    dplyr::filter(series == ID[j])


  #use nighttime data (between midnight and 3am)
  myTemp_data_L0 <- myTemp_data_L0[as.POSIXlt(myTemp_data_L0$ts)$hour<3,]


  myTemp_data_L1 <- proc_L1(data_L0 = myTemp_data_L0,
                          reso = 60,
                          input = "long",
                          date_format ="%Y-%m-%d %H:%M:%S",
                          year = "asis",
                          tz = "GMT")
  #myTemp_data_L1$value <-
  #  baytrends::fillMissing(myTemp_data_L1$value,
  #                         span = 12,
  #                         Dates = NULL,
  #                         max.fill = 12)#gapfillSSA(series = ts, plot.results = FALSE, open.plot = FALSE)


  par(mfrow=c(1,1))
  par(mar = c(5, 5, 5, 5))

  # detect errors
  myDendro_data_L2 <- proc_dendro_L2(dendro_L1 = myDendro_data_L1,
                                   temp_L1 = myTemp_data_L1,
                                   interpol = 12,
                                   #tol_out = 1,
                                   #tol_jump = 2,
                                   plot = TRUE,
                                   plot_export = TRUE,
                                   plot_name = ID[j],
                                   tz="GMT")

  }
}

