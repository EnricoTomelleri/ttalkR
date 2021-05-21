ttgranier <- function(mydata_4D, plot_label) {
  #example call ttgranier(mydata_4D, "split")

  #load required packages
  library(ggplot2)
  #library(Rssa)
  #library(oce)

  #create a color index
  id_col <- mydata_4D$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col))); colnames(id_col_ind) <- c("TT_ID", "ID")
  #create index for color scale
  mydata_4D$id_col_ind <- mydata_4D$TT_ID
  for (i in 1:length(id_col_ind$ID)){
    mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind, mydata_4D$id_col_ind==id_col_ind$TT_ID[i], id_col_ind$ID[i])
  }


  #4.6.Ref & Heat Probes Temperature
  Tref_0C <-
    127.6 - 0.006045 * mydata_4D$Tref_0 + 1.26E-7 * mydata_4D$Tref_0 ^ 2 -
    1.15E-12 * mydata_4D$Tref_0 ^ 3
  #apply a Savitzky-Golay smoothing
  #dfn['Tref_0f'] = savgol_filter(dfn['Tref_0c'], 11, 2,mode='nearest') # window size 5, polynomial order 3


  Tref_0C[Tref_0C < -20] <- NA
  Tref_0C[Tref_0C > 40] <- NA
  #apply a Savitzky-Golay smoothing
  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    ts <- Tref_0C[mydata_4D$TT_ID == ID[j]]
    if (length(ts) < 11) {
      next()
    }
    ts_filt <- savitzkyGolay(ts, 0, 1, 11)
    Tref_0C[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
  }

  #Tref_0C <- na.spline(Tref_0C)
  #Tref_0C <- sgolayfilt(Tref_0C, n=11, p=3)
  #plot(Tref_0C, typ = "l", main = "Tref_0C")
  ############################



  ############################
  Tref_1C <-
    127.6 - 0.006045 * mydata_4D$Tref_1 + 1.26E-7 * mydata_4D$Tref_1 ^ 2 -
    1.15E-12 * mydata_4D$Tref_1 ^ 3

  Tref_1C[Tref_1C < -20] <- NA
  Tref_1C[Tref_1C > 40] <- NA
  #apply a Savitzky-Golay smoothing
  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    ts <- Tref_1C[mydata_4D$TT_ID == ID[j]]
    if (length(ts) < 11) {
      next()
    }
    ts_filt <- savitzkyGolay(ts, 0, 1, 11)
    Tref_1C[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
  }
  #Tref_1C <- na.spline(Tref_1C)
  #Tref_1C <- sgolayfilt(Tref_1C, n=5, p=3)
  #plot(Tref_1C, typ = "l", main = "Tref_1C")
  ############################


  ############################
  Theat_0C <-
    127.6 - 0.006045 * mydata_4D$Theat_0 + 1.26E-7 * mydata_4D$Theat_0 ^ 2 -
    1.15E-12 * mydata_4D$Theat_0 ^ 3

  Theat_0C[Theat_0C < -20] <- NA
  Theat_0C[Theat_0C > 40] <- NA
  #apply a Savitzky-Golay smoothing
  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    ts <- Theat_0C[mydata_4D$TT_ID == ID[j]]
    if (length(ts) < 11) {
      next()
    }
    ts_filt <- savitzkyGolay(ts, 0, 1, 11)
    Theat_0C[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
  }

  #plot(Theat_0C, typ = "l", main = "Theat_0C")
  ############################




  ############################
  Theat_1C <-
    127.6 - 0.006045 * mydata_4D$Theat_1 + 1.26E-7 * mydata_4D$Theat_1 ^ 2 -
    1.15E-12 * mydata_4D$Theat_1 ^ 3

  Theat_1C[Theat_1C < -20] <- NA
  Theat_1C[Theat_1C > 40] <- NA
  #apply a Savitzky-Golay smoothing
  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    ts <- Theat_1C[mydata_4D$TT_ID == ID[j]]
    if (length(ts) < 11) {
      next()
    }
    ts_filt <- savitzkyGolay(ts, 0, 1, 11)
    Theat_1C[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
  }

  #plot(Theat_1C, typ = "l", main = "Theat_1C")
  ############################




  #4.6.1.Sap flow
  dTon <- Theat_1C - Tref_1C
  dToff <- Theat_0C - Tref_0C
  dTmax <-
    (dTon - dToff) #max(Theat_1C-Theat_0C, na.rm=T)#max(Theat_1C-Theat_0C, na.rm=T)

  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:(length(ID))) {
    #dTmax[mydata_4D$TT_ID == ID[j]] <- (dTon[mydata_4D$TT_ID == ID[j]]  - dToff[mydata_4D$TT_ID == ID[j]] )
    df <-
      data.frame(dTmax[mydata_4D$TT_ID == ID[j]], mydata_4D$SDate[mydata_4D$TT_ID == ID[j]])
    colnames(df) <- c("dTmax", "Date")
    if (length(na.omit(df$dTmax)) < 11) {
      next()
    }
    dTmax_day_ID <-
      aggregate(dTmax  ~  Date,
                df,
                max,
                symplify  =  F,
                na.action  =  na.omit)
    daily_Tmax <- rep(NA, length(df$dTmax))
    df <- cbind(df, daily_Tmax)

    for (i in 1:length(df$daily_Tmax)) {
      df$daily_Tmax[i] <- max(df$dTmax[df$Date  ==  df$Date[i]])
    }

    mydata_4D$daily_Tmax[mydata_4D$TT_ID == ID[j]] <- df$daily_Tmax
  }




  Fd <-
    118.99 * ((mydata_4D$daily_Tmax - (dTon - dToff)) / (dTon - dToff)) ^ 1.231 #m^3/s
  Fd[Fd > 1000] <- NA


  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:(length(ID))) {
    ts <- Fd[mydata_4D$TT_ID == ID[j]]
    if (length(ts) < 11) {
      next()
    }
    #Replace missing values in time-series data by interpolation (max gap = 24 hours).
    ts_filt <-
      baytrends::fillMissing(ts,
                             span = 24,
                             Dates = NULL,
                             max.fill = 12)#gapfillSSA(series = ts, plot.results = FALSE, open.plot = FALSE)
    #ts_filt <- ts_filt[[1]]
    #apply a hampel filter
    #ts_filt <- hampel(ts_filt, 24, 3)
    Fd[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]

  }


  #create a data frame for plotting
  df1 <- data.frame(mydata_4D$Timestamp, Fd, mydata_4D$id_col_ind)
  colnames(df1) <- c("Timestamp", "Fd", "id_col_ind")



  if (plot_label == "all_in_one"){
    p <- ggplot(data = df1, aes(Timestamp, Fd)) +
      geom_point(aes(colour = id_col_ind), size = 0.2) +
      #geom_smooth(formula = y ~ s(x, bs = "ds")) +
      geom_smooth() +
      #geom_ma(ma_fun = SMA, n = 1000, color = "red") +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "sap flow (g m-2 s-1)") +
      #labs(title = site) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      ylim(0, quantile(Fd, p = 0.99, na.rm=T))
      print(p)
  }



  if (plot_label == "split"){
    p <- ggplot(data = df1, aes(Timestamp, Fd, color = id_col_ind)) +
      geom_point(aes(group = "whatever"), size = 0.2) +
      #geom_line(aes(group = "whatever")) +
      facet_grid(facets = mydata_4D$TT_ID ~ ., margins = FALSE) +
      geom_smooth(colour = "gray") +
      #binomial_smooth(formula = y ~ splines::ns(x, 2)) +
      labs(x = "Timestamp", y = "sap flow (g m-2 s-1)") +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      ylim(0, quantile(Fd, p = 0.99, na.rm=T))
    print(p)
  }

  if (plot_label == "none"){}


  #df <- data.frame(mydata_4D$Timestamp, Fd, mydata_4D$TT_ID)
  #write.csv(sapFluxD, "../Data/C0200101_SapFluxD.csv")
  #sapFluxD <<- df


  library(lubridate)
  library(dplyr)


  #######aggregate to daily
  #df1$Day <- floor_date(df1$Timestamp, "hour")
  #df2 <- subset(df1, select=c(-Timestamp, -id_col_ind))
  #mydata_daily <- df2 %>%
  #  group_by(Day) %>%
  #  summarize(mean = median(phi, na.rm = TRUE))
  #colnames(mydata_daily) <- c("date", "phi")




}
