#' @export

#create a user function to calculate mode of a data set
# Create the function.
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


ttGrowth <- function(mydata_4D, plot_label) { #this is a beta function
  #example call ttGrowth(mydata_4D, "all_in_one")

  #load required packages
  #library(dendRoAnalyst)
  #library(pracma)
  #library(sarbcurrent)
  #library(changepoint)
  library(lubridate)

  #create a color index
  id_col <- mydata_4D$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col))); colnames(id_col_ind) <- c("TT_ID", "ID")
  #create index for color scale
  mydata_4D$id_col_ind <- mydata_4D$TT_ID
  for (i in 1:length(id_col_ind$ID)){
    mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind, mydata_4D$id_col_ind==id_col_ind$TT_ID[i], id_col_ind$ID[i])
  }



  x <- mydata_4D$growt_sens
  #conversion range according to the manual (September 2020)
  x[x > 85000] <- NA
  x[x < 30000] <- NA
  #f=y0+a*x+b*x^2+c*x^3
  a <- 0.000000008
  b <- -0.0016
  c <- 89.032
  f=a*x^2+b*x+c


  #create a time index for the temporal averaging
  time_index <- paste0(year(mydata_4D$Timestamp), sprintf("%03d", week(mydata_4D$Timestamp)))
  mydata_4D$YYYYww <-  time_index



  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    myDendro_data_L0 <- data.frame(mydata_4D$TT_ID, mydata_4D$Timestamp, mydata_4D$YYYYww, f)#as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01"), f)
    colnames(myDendro_data_L0) <- c("series", "ts", "YYYYww", "dendrometer")




    # Subset dataset for TT_IDs
    myDendro_data_L0 <- myDendro_data_L0 %>%
      dplyr::filter(series == ID[j])
    if (length(na.omit(myDendro_data_L0$dendrometer))<100){next}




    #remove outliers
    t_05 <- quantile(myDendro_data_L0$dendrometer, p=0.05, na.rm=T)
    t_95 <- quantile(myDendro_data_L0$dendrometer, p=0.95, na.rm=T)
    myDendro_data_L0$dendrometer[myDendro_data_L0$dendrometer<t_05] <- NA
    myDendro_data_L0$dendrometer[myDendro_data_L0$dendrometer>t_95] <- NA

    #create a data.frame with Level 1 data
    myDendro_data_L1 <- myDendro_data_L0

    #temporal averaging
    myDendro_data_weekly <- aggregate(dendrometer ~ YYYYww, myDendro_data_L1, median)

    #normalize the radial growth with the initial sensor distance
    myDendro_data_weekly$dendrometer <- (max(myDendro_data_weekly$dendrometer) - myDendro_data_weekly$dendrometer)

    #create a data.frame with Level 2 data
    myDendro_data_L2 <- subset(myDendro_data_L1, select = -dendrometer)

    #merge averaged sharp data with the specific TreeTalker data.frame
    myDendro_data_L2 <- merge(myDendro_data_L2, myDendro_data_weekly, all.x=T)

    #insert processed data into the mydata_4D data.frame
    mydata_4D$dendrometer[mydata_4D$TT_ID == ID[j]] <- myDendro_data_L2$dendrometer
  }


  #create a data frame for plotting
  df1 <- data.frame(mydata_4D$Timestamp, mydata_4D$dendro, mydata_4D$id_col_ind)
  colnames(df1) <- c("Timestamp", "dendrometer", "id_col_ind")



  if (plot_label == "all_in_one"){
    p <- ggplot(data = df1, aes(Timestamp, dendrometer)) +
      geom_point(aes(colour = id_col_ind), size = 0.2, na.rm=T) +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "radial growth (mm)") +
      #labs(title = site) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      ylim(0,3)
      #ylim(quantile(df1$dendrometer, p = 0.01, na.rm=T), quantile(df1$dendrometer, p = 0.99, na.rm=T))
    print(p)
  }



  if (plot_label == "split"){
    p <- ggplot(data = df1, aes(Timestamp, dendrometer, color = id_col_ind)) +
      geom_point(aes(group = "whatever"), size = 0.2, na.rm=T) +
      #geom_line(aes(group = "whatever")) +
      facet_grid(facets = mydata_4D$TT_ID ~ ., margins = FALSE) +
      labs(x = "Timestamp", y = "radial growth (mm)") +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      #ylim(0,3)
      ylim(quantile(df1$dendrometer, p = 0.01, na.rm=T), quantile(df1$dendrometer, p = 0.90, na.rm=T))
    print(p)
  }

  if (plot_label == "none"){}


  #create a data frame for output
  df_ttGrowth <- data.frame(mydata_4D$Timestamp, mydata_4D$dendro, mydata_4D$TT_ID)
  colnames(df_ttGrowth) <- c("Timestamp", "phi", "TT_ID")
  .GlobalEnv$df_ttGrowth <- df_ttGrowth
}
