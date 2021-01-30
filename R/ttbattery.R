ttbattery <- function(mydata_4B, mydata_4D, plot_label){

  HR_Timestamp_4D <- as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01")
  HR_Timestamp_4B <- as.POSIXct(mydata_4B$Timestamp, origin="1970-01-01")
  #create a color index
  id_col <- mydata_4D$IT_ID
  id_col[id_col == max(id_col, na.rm=T)] <- 21
  id_col[id_col != 21] <- id_col[id_col != 21]- max(id_col, na.rm=T)
  mydata_4D$id_col <- abs(id_col)


  #4.8.Battery Voltage
  Bat_mV <- 2*1100*(mydata_4D$adc_Vbat/mydata_4D$adc_bandgap)
  #jpeg(paste("../Figures/Bat_mV_", site, ".jpg", sep=""), width = 1200, height = 800)
  #plot(mydata_4D$Timestamp, Bat_mV, pch=20, ylim=c(3300,4500), xlim=c(min(mydata_4D$Timestamp, na.rm=T),max(mydata_4D$Timestamp, na.rm=T)), main=site, xlab="Timestamp")
  #segments(min(mydata_4D$Timestamp, na.rm=T),3500,max(mydata_4D$Timestamp, na.rm=T),3500, col="red")
  #lines( mydata_4B$Timestamp, mydata_4B$Battery, col="cyan")
  #col_ind <- mydata_4D$Timestamp-min(mydata_4D$Timestamp)
  #plot(HR_Timestamp_4D, Bat_mV, pch=20, ylim=c(3300,4500), xlim=c(min(HR_Timestamp_4D, na.rm=T),max(HR_Timestamp_4D, na.rm=T)), main=site, xlab="Timestamp", col=topo.colors(21)[id_col])
  #segments(min(HR_Timestamp_4D, na.rm=T),3500,max(HR_Timestamp_4D, na.rm=T),3500, col="red")
  #lines(HR_Timestamp_4B, mydata_4B$Battery, col="cyan")
  #segments(15500,0,15500,5000, col="red")
  #dev.off()




  library(ggplot2)
  df <- data.frame(HR_Timestamp_4D, Bat_mV, id_col)
  df1 <-
    data.frame(HR_Timestamp_4B, mydata_4B$Battery)
  colnames(df1) <- c("HR_Timestamp_4B", "Bat_mV")
  p <- ggplot(data = df, aes(HR_Timestamp_4D, Bat_mV))
  p + geom_point(aes(colour = id_col), size = 0.2) +
    scale_color_gradientn(colours = hcl.colors(21, palette = "viridis")) +
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
    theme(legend.position = "none")
  #save the plot
  #ggsave(paste("../Figures/", site, "_BatteryVoltage.png", sep=""),
  #       plot = last_plot(),
  #       width = 10,
  #       height = 7,
  #       units = c("in"),
  #       dpi = 300)
}
