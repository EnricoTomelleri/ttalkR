#' @export

#Air temperature and humidity
#Thermohygrometer (+/-0.1 °C ; +/-2 % ). NXP/Freescale. Model: Si7006


ttTair <- function(mydata_4D, plot_label) {

  #load required packages
  library(ggplot2)

  HR_Timestamp_4D <- mydata_4D$Timestamp#as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01")


  #create a color index
  id_col <- mydata_4D$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col))); colnames(id_col_ind) <- c("TT_ID", "ID")
  #create index for color scale
  mydata_4D$id_col_ind <- mydata_4D$TT_ID
  for (i in 1:length(id_col_ind$ID)){
    mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind, mydata_4D$id_col_ind==id_col_ind$TT_ID[i], id_col_ind$ID[i])
  }


  #create a data frame for plotting
  df <- data.frame(HR_Timestamp_4D, mydata_4D$Tair, mydata_4D$id_col_ind)
  df1 <- data.frame(HR_Timestamp_4D, mydata_4D$Tair/10); colnames(df1) <- c("HR_Timestamp_4B", "Tair")


  if (plot_label == "all_in_one"){
    p <- ggplot(data=df1, aes(x = HR_Timestamp_4D, y = Tair, color=mydata_4D$id_col_ind)) +
      #geom_line(data=df1, aes(HR_Timestamp_4D, Tair/10), color = "grey") +
      geom_point(aes(colour = mydata_4D$id_col_ind), size = 0.2, na.rm=T) +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "Tair (°C)") +
      #labs(title = site) +
      theme(legend.position = "none") +
      scale_x_datetime(minor_breaks=("1 week"))+
      ylim(-20, 40) +
      geom_segment(aes(
        x = min(HR_Timestamp_4D, na.rm = T),
        y = 0,
        xend = max(HR_Timestamp_4D, na.rm = T),
        yend = 0
      ), color = "blue", alpha = 0.1, linetype = 3) +
      geom_segment(aes(
        x = min(HR_Timestamp_4D, na.rm = T),
        y = 35,
        xend = max(HR_Timestamp_4D, na.rm = T),
        yend = 35
      ), color = "red", alpha = 0.1, linetype = 3)
    print(p)
  }

  df1 <- data.frame(mydata_4D$Timestamp, mydata_4D$Tair/10, mydata_4D$id_col_ind)
  colnames(df1) <- c("Timestamp", "Tair", "id_col_ind")

  if (plot_label == "split"){
    p <- ggplot(data = df1, aes(Timestamp, Tair, color = id_col_ind)) +
      geom_point(aes(group = "whatever"), size = 0.4, na.rm = TRUE) +
      geom_line(aes(group = "whatever"), na.rm = TRUE) +
      facet_grid(facets = mydata_4D$TT_ID ~ ., margins = FALSE) +
      #geom_smooth(colour = "gray") +
      #labs(x = "Timestamp", y = "Tair (°C)") +
      labs(x = element_blank(), y = "Tair (°C)") +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      #theme(strip.text.y = element_blank()) + #added for the ttalkR manuscript
      ylim(-10, 40) + #max(df1$Tair, na.rm=T)) +

      geom_segment(aes(
        x = min(Timestamp, na.rm = T),
        y = 0,
        xend = max(Timestamp, na.rm = T),
        yend = 0
      ), color = "blue", alpha = 0.1, linetype = 3) +
      geom_segment(aes(
        x = min(Timestamp, na.rm = T),
        y = 35,
        xend = max(Timestamp, na.rm = T),
        yend = 35
      ), color = "red", alpha = 0.1, linetype = 3)

    print(p)
    p_ttTair <<- p
  }

  if (plot_label == "none"){}

  #create a data frame for output
  df_ttTair <- data.frame(mydata_4D$Timestamp, mydata_4D$Tair/10, mydata_4D$TT_ID)
  colnames(df_ttTair) <- c("Timestamp", "Tair", "TT_ID")
  .GlobalEnv$df_ttTair <- df_ttTair

}







#' @export

ttRH <- function(mydata_4D, plot_label) {

  #load required packages
  library(ggplot2)

  HR_Timestamp_4D <- mydata_4D$Timestamp#as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01")


  #create a color index
  id_col <- mydata_4D$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col))); colnames(id_col_ind) <- c("TT_ID", "ID")
  #create index for color scale
  mydata_4D$id_col_ind <- mydata_4D$TT_ID
  for (i in 1:length(id_col_ind$ID)){
    mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind, mydata_4D$id_col_ind==id_col_ind$TT_ID[i], id_col_ind$ID[i])
  }


  #esclude unreliable values
  mydata_4D$RH[mydata_4D$RH>120] <- NA
  mydata_4D$RH[mydata_4D$RH<20] <- NA

  RH <- mydata_4D$RH

  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:(length(ID))) {
    ts <- RH[mydata_4D$TT_ID == ID[j]]
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
    RH[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]

  }



  #create a data frame for plotting
  df <- data.frame(HR_Timestamp_4D, RH, mydata_4D$id_col_ind)
  df1 <- data.frame(HR_Timestamp_4D, RH); colnames(df1) <- c("HR_Timestamp_4B", "RH")


  if (plot_label == "all_in_one"){
    p <- ggplot(data=df1, aes(x = HR_Timestamp_4D, y = RH, color=mydata_4D$id_col_ind)) +
      #geom_line(data=df1, aes(HR_Timestamp_4D, Tair/10), color = "grey") +
      geom_point(aes(colour = mydata_4D$id_col_ind), size = 0.4, na.rm=T) +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = expression("RH (%)")) +
      #labs(title = site) +
      theme(legend.position = "none") +
      scale_x_datetime(minor_breaks=("1 week"))+
      ylim(0, 120)+
      geom_segment(aes(
        x = min(HR_Timestamp_4D, na.rm = T),
        y = 80,
        xend = max(HR_Timestamp_4D, na.rm = T),
        yend = 80
      ), color = "red")
    print(p)
  }


  df1 <- data.frame(mydata_4D$Timestamp, mydata_4D$RH, mydata_4D$id_col_ind)
  colnames(df1) <- c("Timestamp", "RH", "id_col_ind")

  if (plot_label == "split"){
    p <- ggplot(data = df1, aes(Timestamp, RH, color = id_col_ind)) +
      geom_point(aes(group = "whatever"), size = 0.4, na.rm = TRUE) +
      geom_line(aes(group = "whatever"), na.rm = TRUE) +
      facet_grid(facets = mydata_4D$TT_ID ~ ., margins = FALSE) +
      #geom_smooth(colour = "gray") +
      #labs(x = "Timestamp", y = "RH (%)") +
      labs(x = element_blank(), y = "RH (%)") +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      #theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      theme(strip.text.y = element_blank()) + #added for the ttalkR manuscript
      ylim(min(df1$RH, na.rm=T), max(df1$RH, na.rm=T))

    print(p)
    p_ttRH <<- p
  }

  if (plot_label == "none"){}


  #create a data frame for output
  df_ttRH <- data.frame(mydata_4D$Timestamp, mydata_4D$RH/10, mydata_4D$TT_ID)
  colnames(df_ttRH) <- c("Timestamp", "RH", "TT_ID")
  .GlobalEnv$df_ttRH <- df_ttRH

}

