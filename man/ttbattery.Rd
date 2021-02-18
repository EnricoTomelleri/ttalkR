\name{ttbattery}
\alias{ttbattery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to do check and plot the battery status of a TT site
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ttbattery(mydata_4B, mydata_4D, plot_label)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata_4B}{
%%     ~~Describe \code{mydata_4B} here~~
}
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
%% ~put references to the literature/web site here ~
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
function (mydata_4B, mydata_4D, plot_label)
{
    HR_Timestamp_4D <- as.POSIXct(mydata_4D$Timestamp, origin = "1970-01-01")
    HR_Timestamp_4B <- as.POSIXct(mydata_4B$Timestamp, origin = "1970-01-01")
    id_col <- mydata_4D$IT_ID
    id_col[id_col == max(id_col, na.rm = T)] <- 21
    id_col[id_col != 21] <- id_col[id_col != 21] - max(id_col,
        na.rm = T)
    mydata_4D$id_col <- abs(id_col)
    Bat_mV <- 2 * 1100 * (mydata_4D$adc_Vbat/mydata_4D$adc_bandgap)
    library(ggplot2)
    df <- data.frame(HR_Timestamp_4D, Bat_mV, id_col)
    df1 <- data.frame(HR_Timestamp_4B, mydata_4B$Battery)
    colnames(df1) <- c("HR_Timestamp_4B", "Bat_mV")
    p <- ggplot(data = df, aes(HR_Timestamp_4D, Bat_mV))
    p + geom_point(aes(colour = id_col), size = 0.2) + scale_color_gradientn(colours = hcl.colors(21,
        palette = "viridis")) + geom_line(data = df1, aes(HR_Timestamp_4B,
        Bat_mV), color = "black") + geom_segment(aes(x = min(HR_Timestamp_4D,
        na.rm = T), y = 3500, xend = max(HR_Timestamp_4D, na.rm = T),
        yend = 3500), color = "red") + labs(x = "Timestamp") +
        scale_x_datetime(minor_breaks = ("1 week")) + theme(legend.position = "none")
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
