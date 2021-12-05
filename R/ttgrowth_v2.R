ttGrowth <- function(mydata_4D, plot_label) { #this is a beta function
  #example call ttGrowth(mydata_4D, "all_in_one")

  #load required packages
  #library(dendRoAnalyst)
  library(pracma)
  #library(sarbcurrent)
  library(changepoint)

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



  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    myDendro_data_L0 <- data.frame(mydata_4D$TT_ID, mydata_4D$Timestamp, f)#as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01"), f)
    colnames(myDendro_data_L0) <- c("series", "ts", "value")

    # Subset dataset for TT_IDs
    myDendro_data_L0 <- myDendro_data_L0 %>%
      dplyr::filter(series == ID[j])
    if (length(myDendro_data_L0$value)<100){next}




    #remove outliers
    t_05 <- quantile(myDendro_data_L0$value, p=0.05, na.rm=T)
    t_95 <- quantile(myDendro_data_L0$value, p=0.95, na.rm=T)
    myDendro_data_L0$value[myDendro_data_L0$value<t_05] <- NA
    myDendro_data_L0$value[myDendro_data_L0$value>t_95] <- NA

    #remove missisng values
    #myDendro_data_L0 <- na.omit(subset(myDendro_data_L0, select=-series))
    # Missing value: NA is not allowed in the data as changepoint methods are only sensible for regularly spaced data.
    #Replace missing values in time-series data by interpolation (max gap = 2 weeks).
    myDendro_data_L1 <- myDendro_data_L0
    ts <-
      baytrends::fillMissing(myDendro_data_L0$value,
                             span = 24,
                             Dates = NULL,
                             max.fill = 24*7)
    ts[is.na(ts==T)] <- median(ts, na.rm=T) #replace remaining gaps with ts median

    myDendro_data_L1$value <- ts
    #get nighttime data
    #myDendro_data_L0 <- myDendro_data_L0[as.POSIXlt(myDendro_data_L0$ts)$hour<5,]


    #apply a hampel filter
    #myDendro_data_L0$value

    myDendro_data_MAD <- hampel(myDendro_data_L1$value, 24*7, 2) #weekly time window

    myDendro_data_L1$value <- myDendro_data_MAD$y

    #plot(myDendro_data_L1$value, typ="l")


    #convert sharp distance into growth
    myDendro_data_L2 <- myDendro_data_L1
    #myDendro_data_L1$value <- max(a$y) - a$y
    #myDendro_data_L1$value <- max(myDendro_data_L0$value) - myDendro_data_L0$value



    m_binseg <- cpt.mean(myDendro_data_L2$value, penalty = "BIC", method = "BinSeg", Q = 100)
    bkpnts <- cpts(m_binseg)
    for (k in 1:length(bkpnts)){
      if((k-(24*14))<1){next()}
    ref1 <- median(myDendro_data_L2$value[(bkpnts[k]-(24*14)):(bkpnts[k]-(24*7))])
    ref2 <- median(myDendro_data_L2$value[(bkpnts[k]+(24*7)):(bkpnts[k]+(24*14))])
    myDendro_data_L2$value[bkpnts[k]:length(myDendro_data_L1$value)] <- myDendro_data_L2$value[bkpnts[k]:length(myDendro_data_L2$value)] - (ref2-ref1)
    }


    #plot(myDendro_data_L2$value, typ="l")
    #myDendro_data_L2_spl <- lowess(myDendro_data_L2$value)
    #lines(myDendro_data_L2_spl$y, col="blue")


    #library(dp)

    #a<-hampel(myDendro_data_L1$value, 24, 1)
    #plot(a)

    mydata_4D$dendro[mydata_4D$TT_ID == ID[j]] <- myDendro_data_L2$value
    #mydata_4D$dendro[mydata_4D$TT_ID == ID[j]] <- myDendro_data_L2_spl$y
  }


  #create a data frame for plotting
  df1 <- data.frame(mydata_4D$Timestamp, mydata_4D$dendro, mydata_4D$id_col_ind)
  colnames(df1) <- c("Timestamp", "dendrometer", "id_col_ind")



  if (plot_label == "all_in_one"){
    p <- ggplot(data = df1, aes(Timestamp, dendrometer)) +
      geom_point(aes(colour = id_col_ind), size = 0.2) +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "increment (mm)") +
      #labs(title = site) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      ylim(quantile(df1$dendrometer, p = 0.01, na.rm=T), quantile(df1$dendrometer, p = 0.99, na.rm=T))
    print(p)
  }



  if (plot_label == "split"){
    p <- ggplot(data = df1, aes(Timestamp, dendrometer, color = id_col_ind)) +
      geom_point(aes(group = "whatever"), size = 0.2) +
      #geom_line(aes(group = "whatever")) +
      facet_grid(facets = mydata_4D$TT_ID ~ ., margins = FALSE) +
      labs(x = "Timestamp", y = "increment (mm)") +
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      scale_x_datetime(minor_breaks = ("1 week")) +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      ylim(quantile(df1$dendrometer, p = 0.01, na.rm=T), quantile(df1$dendrometer, p = 0.99, na.rm=T))
    print(p)
  }

  if (plot_label == "none"){}


  #create a data frame for output
  df_ttGrowth <- data.frame(mydata_4D$Timestamp, mydata_4D$dendro, mydata_4D$TT_ID)
  colnames(df_ttGrowth) <- c("Timestamp", "phi", "TT_ID")
  df_ttGrowth <<- df_ttGrowth

}
