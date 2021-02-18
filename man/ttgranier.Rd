\name{ttgranier}
\alias{ttgranier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to process and plot data from Thermal dissipation probes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ttgranier(mydata_4D, plot_label)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata_4D}{
%%     ~~Describe \code{mydata_4D} here~~
}
  \item{plot_label}{
%%     ~~Describe \code{plot_label} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Granier (1985)
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mydata_4D, plot_label)
{
    Tref_0C <- 127.6 - 0.006045 * mydata_4D$Tref_0 + 1.26e-07 *
        mydata_4D$Tref_0^2 - 1.15e-12 * mydata_4D$Tref_0^3
    Tref_0C[Tref_0C < -20] <- NA
    Tref_0C[Tref_0C > 40] <- NA
    ID <- unique(mydata_4D$IT_ID)
    for (j in 1:length(ID)) {
        ts <- Tref_0C[mydata_4D$IT_ID == ID[j]]
        if (length(ts) < 11) {
            (next)()
        }
        ts_filt <- savitzkyGolay(ts, 0, 1, 11)
        Tref_0C[mydata_4D$IT_ID == ID[j]] <- ts_filt[1:length(ts)]
    }
    plot(Tref_0C, typ = "l", main = "Tref_0C")
    Tref_1C <- 127.6 - 0.006045 * mydata_4D$Tref_1 + 1.26e-07 *
        mydata_4D$Tref_1^2 - 1.15e-12 * mydata_4D$Tref_1^3
    Tref_1C[Tref_1C < -20] <- NA
    Tref_1C[Tref_1C > 40] <- NA
    ID <- unique(mydata_4D$IT_ID)
    for (j in 1:length(ID)) {
        ts <- Tref_1C[mydata_4D$IT_ID == ID[j]]
        if (length(ts) < 11) {
            (next)()
        }
        ts_filt <- savitzkyGolay(ts, 0, 1, 11)
        Tref_1C[mydata_4D$IT_ID == ID[j]] <- ts_filt[1:length(ts)]
    }
    plot(Tref_1C, typ = "l", main = "Tref_1C")
    Theat_0C <- 127.6 - 0.006045 * mydata_4D$Theat_0 + 1.26e-07 *
        mydata_4D$Theat_0^2 - 1.15e-12 * mydata_4D$Theat_0^3
    Theat_0C[Theat_0C < -20] <- NA
    Theat_0C[Theat_0C > 40] <- NA
    ID <- unique(mydata_4D$IT_ID)
    for (j in 1:length(ID)) {
        ts <- Theat_0C[mydata_4D$IT_ID == ID[j]]
        if (length(ts) < 11) {
            (next)()
        }
        ts_filt <- savitzkyGolay(ts, 0, 1, 11)
        Theat_0C[mydata_4D$IT_ID == ID[j]] <- ts_filt[1:length(ts)]
    }
    plot(Theat_0C, typ = "l", main = "Theat_0C")
    Theat_1C <- 127.6 - 0.006045 * mydata_4D$Theat_1 + 1.26e-07 *
        mydata_4D$Theat_1^2 - 1.15e-12 * mydata_4D$Theat_1^3
    Theat_1C[Theat_1C < -20] <- NA
    Theat_1C[Theat_1C > 40] <- NA
    ID <- unique(mydata_4D$IT_ID)
    for (j in 1:length(ID)) {
        ts <- Theat_1C[mydata_4D$IT_ID == ID[j]]
        if (length(ts) < 11) {
            (next)()
        }
        ts_filt <- savitzkyGolay(ts, 0, 1, 11)
        Theat_1C[mydata_4D$IT_ID == ID[j]] <- ts_filt[1:length(ts)]
    }
    plot(Theat_1C, typ = "l", main = "Theat_1C")
    dTon <- Theat_1C - Tref_1C
    dToff <- Theat_0C - Tref_0C
    dTmax <- (dTon - dToff)
    ID <- unique(mydata_4D$IT_ID)
    for (j in 1:(length(ID))) {
        df <- data.frame(dTmax[mydata_4D$IT_ID == ID[j]], mydata_4D$SDate[mydata_4D$IT_ID ==
            ID[j]])
        colnames(df) <- c("dTmax", "Date")
        if (length(na.omit(df$dTmax)) < 11) {
            (next)()
        }
        dTmax_day_ID <- aggregate(dTmax ~ Date, df, max, symplify = F,
            na.action = na.omit)
        daily_Tmax <- rep(NA, length(df$dTmax))
        df <- cbind(df, daily_Tmax)
        for (i in 1:length(df$daily_Tmax)) {
            df$daily_Tmax[i] <- max(df$dTmax[df$Date == df$Date[i]])
        }
        mydata_4D$daily_Tmax[mydata_4D$IT_ID == ID[j]] <- df$daily_Tmax
    }
    library(Rssa)
    library(oce)
    Fd <- 118.99 * ((mydata_4D$daily_Tmax - (dTon - dToff))/(dTon -
        dToff))^1.231
    Fd[Fd > 1000] <- NA
    ID <- unique(mydata_4D$IT_ID)
    for (j in 1:(length(ID))) {
        ts <- Fd[mydata_4D$IT_ID == ID[j]]
        if (length(ts) < 11) {
            (next)()
        }
        ts_filt <- baytrends::fillMissing(ts, span = 12, Dates = NULL,
            max.fill = 12)
        Fd[mydata_4D$IT_ID == ID[j]] <- ts_filt[1:length(ts)]
    }
    if (plot_label == T) {
        df <- data.frame(mydata_4D$Timestamp, Fd, mydata_4D$id_col)
        colnames(df) <- c("Timestamp", "Fd", "id_col")
        df$Timestamp <- as.POSIXct(df$Timestamp, origin = "1970-01-01")
        p <- ggplot(data = df, aes(Timestamp, Fd))
        p + geom_point(aes(colour = id_col), size = 0.2) + scale_color_gradientn(colours = hcl.colors(21,
            palette = "viridis")) + labs(x = "Timestamp", y = "sap flow (g m-2 s-1)") +
            scale_x_datetime(minor_breaks = ("1 week")) + theme(legend.position = "none") +
            ylim(0, 50)
    }
    df <- data.frame(mydata_4D$Timestamp, Fd, mydata_4D$IT_ID)
    write.csv(sapFluxD, "../Data/C0200101_SapFluxD.csv")
    sapFluxD <<- df
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
