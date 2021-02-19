\name{ttmeteo}
\alias{ttmeteo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to plot and process environmental data
}
\description{
the function makes use of data frames created by the "ttscrape" function.
}
\usage{
ttmeteo(mydata_4D, plot_label)
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
function (mydata_4D, plot_label)
{
    HR_Timestamp_4D <- as.POSIXct(mydata_4D$Timestamp, origin = "1970-01-01")
    id_col <- mydata_4D$IT_ID
    id_col[id_col == max(id_col, na.rm = T)] <- 21
    id_col[id_col != 21] <- id_col[id_col != 21] - max(id_col,
        na.rm = T)
    mydata_4D$id_col <- abs(id_col)
    df <- data.frame(HR_Timestamp_4D, mydata_4D$RH, mydata_4D$Tair/10,
        id_col)
    colnames(df) <- c("HR_Timestamp_4D", "RH", "Tair")
    p <- ggplot(data = df, aes(HR_Timestamp_4D, RH))
    p + geom_point(aes(colour = id_col), size = 0.2) + scale_color_gradientn(colours = hcl.colors(21,
        palette = "viridis")) + labs(x = "Timestamp", y = "RH (\%)") +
        scale_x_datetime(minor_breaks = ("1 week")) + theme(legend.position = "none")
    df1 <- data.frame(HR_Timestamp_4D[id_col == 21], mydata_4D$Tair[id_col ==
        21])
    colnames(df1) <- c("HR_Timestamp_4D", "Tair")
    p <- ggplot(data = df, aes(HR_Timestamp_4D, Tair))
    p + geom_line(data = df1, aes(HR_Timestamp_4D, Tair/10),
        color = "grey") + geom_point(aes(colour = id_col), size = 0.2) +
        scale_color_gradientn(colours = hcl.colors(21, palette = "viridis")) +
        labs(x = "Timestamp", y = "Tair (°C)") + theme(legend.position = "none") +
        scale_x_datetime(minor_breaks = ("1 week"))
    meteo <<- df
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
