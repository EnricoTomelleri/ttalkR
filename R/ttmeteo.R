
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
      geom_point(aes(colour = mydata_4D$id_col_ind), size = 0.2) +
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


  if (plot_label == "split"){
    p <- ggplot(data=df1, aes(x = HR_Timestamp_4D, y = Tair, color=mydata_4D$id_col_ind)) +
      #geom_line(data=df1, aes(HR_Timestamp_4D, Tair/10), color = "grey") +
      geom_point(aes(colour = mydata_4D$id_col_ind), size = 0.2)  +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "Tair (°C)") +
      theme(legend.position = "none") +
      scale_x_datetime(minor_breaks=("1 week")) +
      facet_grid(facets = mydata_4D$TT_ID ~ ., margins = FALSE) +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
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

  if (plot_label == "none"){}

}









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





  #create a data frame for plotting
  df <- data.frame(HR_Timestamp_4D, mydata_4D$RH, mydata_4D$id_col_ind)
  df1 <- data.frame(HR_Timestamp_4D, mydata_4D$RH); colnames(df1) <- c("HR_Timestamp_4B", "RH")


  if (plot_label == "all_in_one"){
    p <- ggplot(data=df1, aes(x = HR_Timestamp_4D, y = RH, color=mydata_4D$id_col_ind)) +
      #geom_line(data=df1, aes(HR_Timestamp_4D, Tair/10), color = "grey") +
      geom_point(aes(colour = mydata_4D$id_col_ind), size = 0.2) +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "RH (%") +
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


  if (plot_label == "split"){
    p <- ggplot(data=df1, aes(x = HR_Timestamp_4D, y = RH, color=mydata_4D$id_col_ind)) +
      #geom_line(data=df1, aes(HR_Timestamp_4D, Tair/10), color = "grey") +
      geom_point(aes(colour = mydata_4D$id_col_ind), size = 0.2)  +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "RH (%)") +
      theme(legend.position = "none") +
      scale_x_datetime(minor_breaks=("1 week")) +
      facet_grid(facets = mydata_4D$TT_ID ~ ., margins = FALSE) +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      ylim(0, 120)

    print(p)
  }

  if (plot_label == "none"){}

}

