ttbattery <- function(mydata_4B, mydata_4D, plot_label){
  #example call ttbattery(mydata_4B, mydata_4D, "split")

  #load required packages
  library(ggplot2)

  HR_Timestamp_4D <- mydata_4D$Timestamp#as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01")
  HR_Timestamp_4B <- mydata_4B$Timestamp#as.POSIXct(mydata_4B$Timestamp, origin="1970-01-01")


  #create a color index
  id_col <- mydata_4D$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col))); colnames(id_col_ind) <- c("TT_ID", "ID")
  #create index for color scale
  mydata_4D$id_col_ind <- mydata_4D$TT_ID
  for (i in 1:length(id_col_ind$ID)){
    mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind, mydata_4D$id_col_ind==id_col_ind$TT_ID[i], id_col_ind$ID[i])
  }

  #4.8.Battery Voltage
  Bat_mV <- 2*1100*(mydata_4D$adc_Vbat/mydata_4D$adc_bandgap)


  #create a data frame for plotting
  df <- data.frame(HR_Timestamp_4D, Bat_mV, mydata_4D$id_col_ind)
  df1 <- data.frame(HR_Timestamp_4B, mydata_4B$Battery); colnames(df1) <- c("HR_Timestamp_4B", "Bat_mV")

    if (plot_label=="all_in_one"){
    p <- ggplot(data = df, aes(HR_Timestamp_4D, Bat_mV)) +
      geom_point(aes(colour = mydata_4D$id_col_ind), size = 0.2) +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      geom_line(data = df1,
                aes(HR_Timestamp_4B, Bat_mV),
                color = "black") +
      geom_segment(aes(
        x = min(HR_Timestamp_4D, na.rm = T),
        y = 3500,
        xend = max(HR_Timestamp_4D, na.rm = T),
        yend = 3500
      ), color = "red") +
      labs(x = "Timestamp") +
      #labs(title = site) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      ylim(quantile(Bat_mV, p = 0.01, na.rm=T), (quantile(Bat_mV, p = 0.99, na.rm=T)+100))
    #save the plot
    #ggsave(paste("../Figures/", site, "_BatteryVoltage.png", sep=""),
    #       plot = last_plot(),
    #       width = 10,
    #       height = 7,
    #       units = c("in"),
    #       dpi = 300)
    print(p)
  }

  if (plot_label=="split"){
    p <- ggplot(data=df, aes(x=HR_Timestamp_4D, y=Bat_mV, color=mydata_4D$id_col_ind)) +
      geom_point(aes(group = "whatever"), size = 0.2) +
      #geom_line(aes(group = "whatever")) +
      facet_grid(facets = mydata_4D$TT_ID ~ ., margins = FALSE) +
      labs(x = "Timestamp") +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      geom_segment(aes(
        x = min(HR_Timestamp_4D, na.rm = T),
        y = 3500,
        xend = max(HR_Timestamp_4D, na.rm = T),
        yend = 3500
      ), color = "red", alpha = 0.1, linetype = 3) +
      ylim(quantile(Bat_mV, p = 0.01, na.rm=T), quantile(Bat_mV, p = 0.99, na.rm=T))

    print(p)
  }

  if (plot_label == "none"){}

  #create a data frame for output
  df_ttbattery <- data.frame(mydata_4D$Timestamp, Bat_mV, mydata_4D$TT_ID)
  colnames(df_ttbattery) <- c("Timestamp", "Bat_mV", "TT_ID")
  df_ttbattery <<- df_ttbattery



}
