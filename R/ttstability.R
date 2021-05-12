ttstability <- function(mydata_4D, plot_label){


  #load required packages
  library(ggplot2)



  HR_Timestamp_4D <- mydata_4D$Timestamp
  #create a color index
  id_col <- mydata_4D$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col))); colnames(id_col_ind) <- c("TT_ID", "ID")
  #create index for color scale
  mydata_4D$id_col_ind <- mydata_4D$TT_ID
  for (i in 1:length(id_col_ind$ID)){
    mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind, mydata_4D$id_col_ind==id_col_ind$TT_ID[i], id_col_ind$ID[i])
  }


  #4.11.Tree Stability
  Ax <- mydata_4D$gx_mean
  Ay <- mydata_4D$gy_mean
  Az <- mydata_4D$gz_mean
  phi <- tanh((sqrt(Ax^2+Ay^2))/Az)
  #plot(phi, ylim=c(-0.5, -0.2))
  phi[phi < -0.8] <- NA

  rad2deg <- function(rad) {(rad * 180) / (pi)}
  deg2rad <- function(deg) {(deg * pi) / (180)}

  tiltangle <- 20 # in degree
  phi <- rad2deg(tanh((sqrt(Ax^2+Ay^2))/Az)) + tiltangle



  df1 <- data.frame(HR_Timestamp_4D, phi); colnames(df1) <- c("HR_Timestamp_4B", "phi")

  if (plot_label == "all_in_one"){
  p <- ggplot(data=df1, aes(HR_Timestamp_4D, phi)) +
    geom_point(aes(colour = mydata_4D$id_col_ind), size = 0.2) +
    scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
    labs(x = "Timestamp", y = "phi (degrees)") +
    #labs(title = site) +
    theme(legend.position = "none") +
    scale_x_datetime(minor_breaks=("1 week")) +
    ylim(-20,20)
 print(p)



  }


  if (plot_label == "split"){
    p <- ggplot(data=df1, aes(x = HR_Timestamp_4D, y = phi, color=mydata_4D$id_col_ind)) +
      geom_point(aes(colour = mydata_4D$id_col_ind), size = 0.2)  +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "phi (degrees)") +
      theme(legend.position = "none") +
      scale_x_datetime(minor_breaks=("1 week")) +
      facet_grid(facets = mydata_4D$TT_ID ~ ., margins = FALSE) +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      ylim(-20, 20)+
      geom_segment(aes(
        x = min(HR_Timestamp_4D, na.rm = T),
        y = 0,
        xend = max(HR_Timestamp_4D, na.rm = T),
        yend = 0
      ), color = "red")

    print(p)
  }

  if (plot_label == "none"){}

}








#trunk_axis_movement
Dx <- mydata_4D$gx_sd
Dy <- mydata_4D$gy_sd
Dz <- mydata_4D$gz_sd

tam <- Dx + Dy + Dz
p <- ggplot(data=df, aes(HR_Timestamp_4D, tam))
p + geom_point(aes(colour = id_col), size = 0.2)  +
  scale_color_gradientn(colours = hcl.colors(21, palette = "viridis")) +
  labs(x = "Timestamp", y = "trunk_axis_movement") +
  #labs(title = site) +
  theme(legend.position = "none") +
  scale_x_datetime(minor_breaks=("1 week")) +
  ylim(-0.0000001,0.0000001)
#ylim(quantile(phi, probs = 0.001), quantile(phi, probs = 0.999))
#plot(mydata_4D$Tair/10, pch=20, ylab="Air Temperature (°C)")
#save the plot
ggsave(paste("../Figures/", site, "_tam.png", sep=""),
       plot = last_plot(),
       width = 10,
       height = 7,
       units = c("in"),
       dpi = 300)

