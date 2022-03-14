#' @export

ttStability <- function(mydata_4D, plot_label){

  #Treetalker is able to record oscillation of tree due to gravity with Spherical Coordinate System.
  #With basic trigonometry, φ, the angle between the gravity vector and the z-axis can be assessed by using equation 10 (Fisher, 2010).
  #This capability will improve the monitoring of forest ecosystem resilience against wind impact.

  #Accelerometer (± 0.01°) Manufacturer:NXP/Freescale. Model: Si7006

  #load required packages
  library(ggplot2)


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


  #create a data frame for plotting
  df1 <- data.frame(mydata_4D$Timestamp, phi, mydata_4D$id_col_ind)
  colnames(df1) <- c("Timestamp", "phi", "id_col_ind")

  if (plot_label == "all_in_one"){
    p <- ggplot(data=df1, aes(Timestamp, phi)) +
      geom_point(aes(colour = id_col_ind), size = 0.2, na.rm=T) +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "phi (degrees)") +
      #labs(title = site) +
      theme(legend.position = "none") +
      scale_x_datetime(minor_breaks=("1 week")) +
      ylim(quantile(phi, p = 0.01, na.rm=T), quantile(phi, p = 0.99, na.rm=T))
    print(p)



  }


  if (plot_label == "split"){
    p <- ggplot(data=df1, aes(x = Timestamp, y = phi, color=id_col_ind)) +
      geom_point(aes(colour = id_col_ind), size = 0.4, na.rm=T)  +
      geom_line(aes(group = "whatever"), na.rm = TRUE) +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "phi (degrees)") +
      theme(legend.position = "none") +
      scale_x_datetime(minor_breaks=("1 week")) +
      facet_grid(facets = mydata_4D$TT_ID ~ ., margins = FALSE) +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      ylim(quantile(phi, p = 0.01, na.rm=T), quantile(phi, p = 0.99, na.rm=T)) +

      geom_segment(aes(
        x = min(Timestamp, na.rm = T),
        y = quantile(phi, p = 0.5, na.rm=T),
        xend = max(Timestamp, na.rm = T),
        yend = quantile(phi, p = 0.5, na.rm=T)),
        color = "red", alpha = 0.1, linetype = 3)

    print(p)
  }

  if (plot_label == "none"){}


  #create a data frame for output
  df_ttStability <- data.frame(mydata_4D$Timestamp, phi, mydata_4D$TT_ID)
  colnames(df_ttStability) <- c("Timestamp", "phi", "TT_ID")
  .GlobalEnv$df_ttStability <- df_ttStability
}



