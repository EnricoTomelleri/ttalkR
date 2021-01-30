ttmeteo <- function(mydata_4D, plot_label) {

  HR_Timestamp_4D <- as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01")

  #create a color index
  id_col <- mydata_4D$IT_ID
  id_col[id_col == max(id_col, na.rm=T)] <- 21
  id_col[id_col != 21] <- id_col[id_col != 21]- max(id_col, na.rm=T)
  mydata_4D$id_col <- abs(id_col)

  #4.10.Meteorological Data
  df <- data.frame(HR_Timestamp_4D, mydata_4D$RH, mydata_4D$Tair/10, id_col)
  colnames(df) <- c("HR_Timestamp_4D", "RH", "Tair")





  p <- ggplot(data=df, aes(HR_Timestamp_4D, RH))
  p + geom_point(aes(colour = id_col), size = 0.2)  +
    scale_color_gradientn(colours = hcl.colors(21, palette = "viridis")) +
    labs(x = "Timestamp", y = "RH (%)") +
    #labs(title = site) +
    scale_x_datetime(minor_breaks=("1 week")) +
    theme(legend.position = "none")
  #plot(mydata_4D$RH, pch=20, ylab="relative humidity (%)")
  #save the plot
  #ggsave(paste("../Figures/", site, "_RH.png", sep=""),
  #       plot = last_plot(),
  #       width = 10,
  #       height = 7,
  #       units = c("in"),
  #       dpi = 300)


  df1 <- data.frame(HR_Timestamp_4D[id_col == 21], mydata_4D$Tair[id_col == 21]); colnames(df1) <- c("HR_Timestamp_4D", "Tair")
  p <- ggplot(data=df, aes(HR_Timestamp_4D, Tair))
  p + geom_line(data=df1, aes(HR_Timestamp_4D, Tair/10), color = "grey") +
    geom_point(aes(colour = id_col), size = 0.2)  +
    scale_color_gradientn(colours = hcl.colors(21, palette = "viridis")) +
    labs(x = "Timestamp", y = "Tair (°C)") +
    #labs(title = site) +
    theme(legend.position = "none") +
    scale_x_datetime(minor_breaks=("1 week"))

  #plot(mydata_4D$Tair/10, pch=20, ylab="Air Temperature (°C)")
  #save the plot
  #ggsave(paste("../Figures/", site, "_Tair.png", sep=""),
  #       plot = last_plot(),
  #       width = 10,
  #       height = 7,
  #       units = c("in"),
  #       dpi = 300)

  meteo <<- df

}
