ttgrowth <- function(mydata_4D) {

  #load required packages
  library(dendRoAnalyst)
  library(pracma)


  x <- mydata_4D$growt_sens
  #conversion range according to the manual (September 2020)
  x[x > 85000] <- NA
  x[x < 30000] <- NA
  #f=y0+a*x+b*x^2+c*x^3
  a <- 0.000000008
  b <- -0.0016
  c <- 89.032
  f=a*x^2+b*x+c
  #a <- 237908.4541
  #b <- -1.1171
  #c <- 199.433

  #f=(a+b*x)/(c+x)

  #millimeters to
  #f <- f*10
  # to invert since growth equal less distance from trunk
  #f <- 40-f
  #remove baseline distance
  #ID <- unique(mydata_4D$TT_ID)
  #for (j in 1:length(ID)) {
  #  ts <- f[mydata_4D$TT_ID == ID[j]]
  #  if (length(ts) < 11) {
  #    next()
  #  }
  #  ts_filt <- ts-min(ts, na.rm=T)
  #  f[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
  #  #millimiters to micron
  #}



  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    myDendro_data_L0 <- data.frame(mydata_4D$TT_ID, mydata_4D$Timestamp, f)#as.POSIXct(mydata_4D$Timestamp, origin="1970-01-01"), f)
    colnames(myDendro_data_L0) <- c("series", "ts", "value")

    # Subset dataset for example
    myDendro_data_L0 <- myDendro_data_L0 %>%
      dplyr::filter(series == ID[j])





    #remove outliers
    t_05 <- quantile(myDendro_data_L0$value, p=0.05, na.rm=T)
    t_95 <- quantile(myDendro_data_L0$value, p=0.95, na.rm=T)
    myDendro_data_L0$value[myDendro_data_L0$value<t_05] <- NA
    myDendro_data_L0$value[myDendro_data_L0$value>t_95] <- NA

    #remove missisng values
    myDendro_data_L0 <- na.omit(subset(myDendro_data_L0, select=-series))




    #get nighttime data
    #myDendro_data_L0 <- myDendro_data_L0[as.POSIXlt(myDendro_data_L0$ts)$hour<5,]


    #apply a hampel filter
    #myDendro_data_L0$value
    myDendro_data_L1 <- myDendro_data_L0
    myDendro_data_MAD <- hampel(myDendro_data_L0$value, 24*7, 3) #weekly time window

    myDendro_data_L1$value <- myDendro_data_MAD$y

    plot(myDendro_data_L1$value, typ="l")


    #convert sharp distance into growth
    myDendro_data_L2 <- myDendro_data_L1
    #myDendro_data_L1$value <- max(a$y) - a$y
    #myDendro_data_L1$value <- max(myDendro_data_L0$value) - myDendro_data_L0$value


    library(sarbcurrent)
    m_binseg <- cpt.mean(myDendro_data_L2$value, penalty = "BIC", method = "BinSeg", Q = 25)
    bkpnts <- cpts(m_binseg)
    for (k in 1:length(bkpnts)){
    ref1 <- median(myDendro_data_L2$value[(bkpnts[k]-(24*14)):(bkpnts[k]-(24*7))])
    ref2 <- median(myDendro_data_L2$value[(bkpnts[k]+(24*7)):(bkpnts[k]+(24*14))])
    myDendro_data_L2$value[bkpnts[k]:length(myDendro_data_L1$value)] <- myDendro_data_L2$value[bkpnts[k]:length(myDendro_data_L2$value)] - (ref2-ref1)
    }


    plot(myDendro_data_L2$value, typ="l")
    myDendro_data_L1_spl <- lowess(myDendro_data_L2$value)
    lines(myDendro_data_L2_spl$y, col="blue")


    library(dp)

    #a<-hampel(myDendro_data_L1$value, 24, 1)
    #plot(a)


    myDendro_data_L1 <- ungroup(myDendro_data_L1)



    daily <- daily.data(jump_free, TreeNum=1)

    zg.phase<-phase.zg(df=daily, TreeNum=1, outputplot=TRUE, days=c(150,160))
  }

}
