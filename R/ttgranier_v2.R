# fix ttgranier function with Sahla one

library(ttalkR)
library(prospectr)

# Rename and edit the function
ttGranier_v2 <- function (mydata_4D, plot_label)
  {
  id_col <- mydata_4D$TT_ID
  id_col_ind <- data.frame(unique(id_col), 1:length(unique(id_col)))
  colnames(id_col_ind) <- c("TT_ID", "ID")
  mydata_4D$id_col_ind <- mydata_4D$TT_ID
  for (i in 1:length(id_col_ind$ID)) {
    mydata_4D$id_col_ind <- replace(mydata_4D$id_col_ind,
                                    mydata_4D$id_col_ind == id_col_ind$TT_ID[i], id_col_ind$ID[i])
  }
  Tref_0C <- 127.6 - 0.006045 * mydata_4D$Tref_0 + 1.26e-07 *
    mydata_4D$Tref_0^2 - 1.15e-12 * mydata_4D$Tref_0^3
  Tref_0C[Tref_0C < -20] <- NA
  Tref_0C[Tref_0C > 50] <- NA
  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    ts <- Tref_0C[mydata_4D$TT_ID == ID[j]]
    if (length(na.omit(ts)) < 23) {
      next()
    }

    #------------------------
    # Check for missing value using is.na() at the beginning of ts
    if (is.na(ts[1])) {
      # Replace the missing value with the first non-missing value
      ts[1] <- na.omit(ts)[1]
    }
    # Check for missing value using is.na() at the end of ts
    if (is.na(ts[length(ts)])) {
      # Replace the missing value with the first non-missing value
      ts[length(ts)] <- na.omit(ts)[length(na.omit(ts))]
    }
    # Apply na.approx() directly to the time series without using zoo::na.approx()
    ts_filt <- signal::sgolayfilt(na.approx(ts, maxgap=100000), p = 1, n = 13)

    #reinsert missing values
    ts_filt[is.na(ts) == T] <- NA

    # Update mydata_4D$Tref_1C where the condition is met
    mydata_4D$Tref_0C[mydata_4D$TT_ID == ID[j]] <- ts_filt
    #------------------------
  }
  Tref_1C <- 127.6 - 0.006045 * mydata_4D$Tref_1 + 1.26e-07 *
    mydata_4D$Tref_1^2 - 1.15e-12 * mydata_4D$Tref_1^3
  Tref_1C[Tref_1C < -20] <- NA
  Tref_1C[Tref_1C > 50] <- NA
  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    ts <- Tref_1C[mydata_4D$TT_ID == ID[j]]
    if (length(na.omit(ts)) < 23) {
      next()
    }
    #------------------------
    # Check for missing value using is.na() at the beginning of ts
    if (is.na(ts[1])) {
      # Replace the missing value with the first non-missing value
      ts[1] <- na.omit(ts)[1]
    }
    # Check for missing value using is.na() at the end of ts
    if (is.na(ts[length(ts)])) {
      # Replace the missing value with the first non-missing value
      ts[length(ts)] <- na.omit(ts)[length(na.omit(ts))]
    }
    # Apply na.approx() directly to the time series without using zoo::na.approx()
    ts_filt <- signal::sgolayfilt(na.approx(ts, maxgap=100000), p = 1, n = 13)

    #reinsert missing values
    ts_filt[is.na(ts) == T] <- NA

    # Update mydata_4D$Tref_1C where the condition is met
    mydata_4D$Tref_1C[mydata_4D$TT_ID == ID[j]] <- ts_filt
    #------------------------
  }
  Theat_0C <- 127.6 - 0.006045 * mydata_4D$Theat_0 + 1.26e-07 *
    mydata_4D$Theat_0^2 - 1.15e-12 * mydata_4D$Theat_0^3
  Theat_0C[Theat_0C < -20] <- NA
  Theat_0C[Theat_0C > 50] <- NA
  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    ts <- Theat_0C[mydata_4D$TT_ID == ID[j]]
    if (length(na.omit(ts)) < 23) {
      next()
    }
    #------------------------
    # Check for missing value using is.na() at the beginning of ts
    if (is.na(ts[1])) {
      # Replace the missing value with the first non-missing value
      ts[1] <- na.omit(ts)[1]
    }
    # Check for missing value using is.na() at the end of ts
    if (is.na(ts[length(ts)])) {
      # Replace the missing value with the first non-missing value
      ts[length(ts)] <- na.omit(ts)[length(na.omit(ts))]
    }
    # Apply na.approx() directly to the time series without using zoo::na.approx()
    ts_filt <- signal::sgolayfilt(na.approx(ts, maxgap=100000), p = 1, n = 13)

    #reinsert missing values
    ts_filt[is.na(ts) == T] <- NA

    # Update mydata_4D$Tref_1C where the condition is met
    mydata_4D$Theat_0C[mydata_4D$TT_ID == ID[j]] <- ts_filt
    #------------------------
  }
  Theat_1C <- 127.6 - 0.006045 * mydata_4D$Theat_1 + 1.26e-07 *
    mydata_4D$Theat_1^2 - 1.15e-12 * mydata_4D$Theat_1^3
  Theat_1C[Theat_1C < -20] <- NA
  Theat_1C[Theat_1C > 50] <- NA
  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:length(ID)) {
    ts <- Theat_1C[mydata_4D$TT_ID == ID[j]]
    if (length(na.omit(ts)) < 23) {
      next()
    }
    #------------------------
    # Check for missing value using is.na() at the beginning of ts
    if (is.na(ts[1])) {
      # Replace the missing value with the first non-missing value
      ts[1] <- na.omit(ts)[1]
    }
    # Check for missing value using is.na() at the end of ts
    if (is.na(ts[length(ts)])) {
      # Replace the missing value with the first non-missing value
      ts[length(ts)] <- na.omit(ts)[length(na.omit(ts))]
    }
    # Apply na.approx() directly to the time series without using zoo::na.approx()
    ts_filt <- signal::sgolayfilt(na.approx(ts, maxgap=100000), p = 1, n = 13)

    #reinsert missing values
    ts_filt[is.na(ts) == T] <- NA

    # Update mydata_4D$Tref_1C where the condition is met
    mydata_4D$Theat_1C[mydata_4D$TT_ID == ID[j]] <- ts_filt
    #------------------------
  }

  #
  #
  #


  mydata_4D$dTon <- mydata_4D$Theat_1C - mydata_4D$Tref_1C # end of heating
  mydata_4D$dToff <- mydata_4D$Theat_0C - mydata_4D$Tref_0C # before heating
  #mydata_4D$dTo <- rep(NA, length(mydata_4D$dToff))
  dTmax <- (mydata_4D$dTon -  mydata_4D$dToff)

  #
  #
  #

  #ID <- unique(mydata_4D$TT_ID)
  #for (j in 1:(length(ID))) {
  #dTmax[mydata_4D$TT_ID == ID[j]] <- (mydata_4D$dTon[mydata_4D$TT_ID == ID[j]]  - mydata_4D$dToff[mydata_4D$TT_ID == ID[j]] )
  #df <-       data.frame(dTmax[mydata_4D$TT_ID == ID[j]], mydata_4D$SDate[mydata_4D$TT_ID == ID[j]])
  #colnames(df) <- c("dTmax", "Date")

  #if (length(na.omit(df$dTmax)) < 11) {
  #  next()     }
  #dTmax_day_ID <- aggregate(dTmax  ~  Date,df, max,symplify  =  F, na.action  =  na.omit)
  #daily_Tmax <- rep(NA, length(df$dTmax))
  #df <- cbind(df, daily_Tmax)
  #for (i in 1:length(df$daily_Tmax)) {
  #df$daily_Tmax[i] <- max(df$dTmax[df$Date  ==  df$Date[i]])
  #}
  #mydata_4D$daily_Tmax[mydata_4D$TT_ID == ID[j]] <- df$daily_Tmax
  #}

  #
  #
  #

#  ID <- unique(mydata_4D$TT_ID)
#  for (j in 1:(length(ID))) {
#    df <-       data.frame(mydata_4D$dTon[mydata_4D$TT_ID == ID[j]], mydata_4D$dToff[mydata_4D$TT_ID == ID[j]], mydata_4D$SDate[mydata_4D$TT_ID == ID[j]], mydata_4D$STime[mydata_4D$TT_ID == ID[j]])
#    colnames(df) <- c("dTon", "dToff", "Date", "Time")
#
#    if (length(na.omit(df$dTmax)) < 11) {
#      next()     }
#
#    dTo <- aggregate((dTon[Time < "03:00:00"] - dToff[Time < "03:00:00"])  ~  Date[Time < "03:00:00"], df, max, symplify  =  F, na.action  =  na.omit)
#    colnames(dTo) <- c("Date", "dTo")
#
#    daily_To <- rep(NA, length(df$dTon))
#    df <- cbind(df, daily_To)
#    for (i in 1:length(df$daily_To)) {
#      if (any(dTo$Date  ==  df$Date[i]) == F) {next()}
#      df$daily_To[i] <- dTo$dTo[dTo$Date  ==  df$Date[i]]
#    }
#    mydata_4D$dTo[mydata_4D$TT_ID == ID[j]] <- df$daily_To
#  }

  #new version
  ID <- unique(mydata_4D$TT_ID)
  for (j in 1:(length(ID))) {
    df <- data.frame(
      dTon = mydata_4D$dTon[mydata_4D$TT_ID == ID[j]],
      dToff = mydata_4D$dToff[mydata_4D$TT_ID == ID[j]],
      Date = mydata_4D$AcDate[mydata_4D$TT_ID == ID[j]],
      Time = mydata_4D$AcTime[mydata_4D$TT_ID == ID[j]]
    )
    colnames(df) <- c("dTon", "dToff", "Date", "AcTime")

    if (sum(!is.na(df$dToff)) < 11) {
      next()
    }

    dTo <- aggregate(dTon[AcTime < "03"] - dToff[AcTime < "03"] ~ Date[AcTime < "03"], df, max, simplify = FALSE, na.action = na.omit)
    colnames(dTo) <- c("Date", "dTo")

    daily_To <- rep(NA, length(df$dTon))
    df <- cbind(df, daily_To)
    for (i in 1:length(df$daily_To)) {
      if (!any(dTo$Date == df$Date[i])) {
        next()
      }
      df$daily_To[i] <- dTo$dTo[dTo$Date == df$Date[i]]
    }
    mydata_4D$dTo[mydata_4D$TT_ID == ID[j]] <- df$daily_To
  }
  #
  #
  #

  #dTo <- mydata_4D$daily_Tmax # dTo
  #dTu <- dTmax # questo è il segnale a un dato valore di flusso, va estratto credo per ogni lettura di ogni tree talker?

  mydata_4D$dTo[sapply(mydata_4D$dTo, is.null)] <- NA
  mydata_4D$dTo <- as.numeric(unlist(mydata_4D$dTo, use.names = F)) #under no flux conditions (nighttime?)
  mydata_4D$dTu <- as.numeric(unlist(mydata_4D$dTon - mydata_4D$dToff)) #for every half an hour
  #
  # Calculate K
  #


  #head(dTo)
  #head(dTu)

  # clean_data$K1 <- ((clean_data$DT_max24h / clean_data$DT) -1) # FORMULA SHALA LINEA 280

  #K = (dTo / dTu) - 1 # modified by E
  #K <- (dTo - dTu) / dTu
  K <- (mydata_4D$dTo - mydata_4D$dTu) / mydata_4D$dTu

  #When temperature is lower than 0, assume K1 is 0, as the equation might
  #yield negative values
  K <- ifelse(K < 0, 0, K)

  #
  #
  #

  #ID <- unique(mydata_4D$TT_ID)
  #for (j in 1:(length(ID))) {
  #  print(head(df))
  #  df <- data.frame(dTmax[mydata_4D$TT_ID == ID[j]], mydata_4D$SDate[mydata_4D$TT_ID ==
  #                                                                      ID[j]])
  #  colnames(df) <- c("dTmax", "Date")
  #  if (length(na.omit(df$dTmax)) < 11) {
  #    (next)()
  #  }
  #  dTmax_day_ID <- aggregate(dTmax ~ Date, df, max, symplify = F,
  #                            na.action = na.omit)
  #  daily_Tmax <- rep(NA, length(df$dTmax))
  #  df <- cbind(df, daily_Tmax)
  #  for (i in 1:length(df$daily_Tmax)) {
  #    df$daily_Tmax[i] <- max(df$dTmax[df$Date == df$Date[i]])
  #  }
  #  mydata_4D$daily_Tmax[mydata_4D$TT_ID == ID[j]] <- df$daily_Tmax
  #}


  #mydata_4D$Fd <- 118.99 * ((mydata_4D$daily_Tmax - (dTon - dToff))/(dTon -
  #                                                           dToff))^1.231
  #Fd_Do = (11.3 * (K / (1 - K))) ^ 0.707 # new Fd
  #mydata_4D$Fd_Do <- (11.3 * K / (1 - K)) ^ 0.707 # new Fd
  mydata_4D$Fd_Do <- 4.2841 * K ^ 1.231
  #
  #
  #

  #Fd[Fd > 1000] <- NA
  #Fd_Do[Fd_Do > 1000] <- NA  # Assuming a  range for Fd_Do, adjusted according to literature / to be double checked
  #ID <- unique(mydata_4D$TT_ID)
  #for (j in 1:(length(ID))) {
  #  ts <- Fd[mydata_4D$TT_ID == ID[j]]
  #  ts_ldr <- Fd_Do[mydata_4D$TT_ID == ID[j]]  # # # # # # #  NEW LINE

  #  if (length(ts) < 11) {
  #    (next)()
  #  }
  #  ts_filt <- baytrends::fillMissing(ts, span = 24, Dates = NULL,
  #                                    max.fill = 12)
  #  ts_ldr_filt <- baytrends::fillMissing(ts_ldr, span = 24, Dates = NULL, max.fill = 12) # # # # # # #  NEW LINE

  # Fd[mydata_4D$TT_ID == ID[j]] <- ts_filt[1:length(ts)]
  # Fd_Do[mydata_4D$TT_ID == ID[j]] <- ts_ldr_filt[1:length(ts_ldr)] # # # # # # #  NEW LINE

  #}
  df1 <- data.frame(mydata_4D$Timestamp, mydata_4D$Fd_Do, mydata_4D$id_col_ind) # # # # # # #  NEW LINE
  colnames(df1) <- c("Timestamp", "Fd_Do", "id_col_ind") # # # # # # #  NEW LINE

  if (plot_label == "all_in_one") {
    p <- ggplot(data = df1) +
      #geom_point(aes(x = Timestamp, y = Fd, colour = id_col_ind), size = 0.2, na.rm = TRUE) +
      geom_point(aes(x = Timestamp, y = Fd_Do, colour = id_col_ind), shape = 2, size = 0.2, na.rm = TRUE) + # # # # # # #  NEW LINE
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      labs(x = "Timestamp", y = "Sap flow density / l*dm-2*h-1") + # # # # # # #  NEW LINE
      scale_x_datetime(minor_breaks = "1 day") +
      theme(legend.position = "none") +
      ylim(0, quantile(df1$Fd_Do, 0.95, na.rm=T))

    print(p)
  }

  if (plot_label == "split") {
    p <- ggplot(data = df1) +
      # Point plot for Fd
      #geom_point(aes(x = Timestamp, y = Fd, colour = id_col_ind), size = 0.4, na.rm = TRUE) +
      #geom_line(aes(x = Timestamp, y = Fd, group = id_col_ind), na.rm = TRUE) +
      # Point plot for Fd_Do
      geom_point(aes(x = Timestamp, y = Fd_Do, colour = id_col_ind), shape = 2, size = 0.4, na.rm = TRUE) +# # # # # # #  NEW LINE
      geom_line(aes(x = Timestamp, y = Fd_Do, group = id_col_ind), na.rm = TRUE) +# # # # # # #  NEW LINE
      # Facet grid for each TT_ID
      facet_grid(facets = mydata_4D$TT_ID ~ ., scales = "free", margins = FALSE) +
      # Labels and other aesthetics
      labs(x = element_blank(), y = "Sap flow density / l*dm-2*h-1") +# # # # # # #  NEW LINE
      scale_color_gradientn(colours = hcl.colors(30, palette = "viridis")) +
      scale_x_datetime(minor_breaks = "1 day") +
      theme(legend.position = "none") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
      ylim(0, quantile(df1$Fd_Do, 0.95, na.rm=T))

    print(p)
    p_ttGranier <<- p
  }
  library(lubridate)
  library(dplyr)
  #df_ttGranier <- data.frame(mydata_4D$Timestamp, Fd, mydata_4D$TT_ID)
  #colnames(df_ttGranier) <- c("Timestamp", "Fd", "TT_ID")

  #
  #
  #

  df_ttGranier <- data.frame(mydata_4D$Timestamp, mydata_4D$Fd_Do, mydata_4D$TT_ID)# # # # # # #  NEW LINE
  colnames(df_ttGranier) <- c("Timestamp", "Fd_Do", "TT_ID")# # # # # # #  NEW LINE

  #
  #
  #

   .GlobalEnv$df_ttGranier <- df_ttGranier
}

