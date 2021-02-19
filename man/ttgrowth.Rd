\name{ttgrowth}
\alias{ttgrowth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to process and plot dendrometer data
}
\description{
the function makes use of data frames created by the "ttscrape" function. It is makes use of the "treenetproc" R-package for analyzing the Sharp data within a mesh of TreeTalkers.
}
\usage{
ttgrowth(mydata_4D)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata_4D}{
%%     ~~Describe \code{mydata_4D} here~~
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
function (mydata_4D)
{
    x <- mydata_4D$growt_sens
    x[x > 850000] <- NA
    x[x < 40000] <- NA
    a <- 237908.4541
    b <- -1.1171
    c <- 199.433
    f = (a + b * x)/(c + x)
    f <- f * 10000
    f <- 1e+05 - f
    ID <- unique(mydata_4D$IT_ID)
    for (j in 1:length(ID)) {
        myDendro_data_L0 <- data.frame(mydata_4D$IT_ID, as.POSIXct(mydata_4D$Timestamp,
            origin = "1970-01-01"), f)
        colnames(myDendro_data_L0) <- c("series", "ts", "value")
        myDendro_data_L0 <- myDendro_data_L0 \%>\% dplyr::filter(series ==
            ID[j])
        myDendro_data_L1 <- proc_L1(data_L0 = myDendro_data_L0,
            reso = 240, input = "long", date_format = "\%Y-\%m-\%d \%H:\%M:\%S",
            year = "asis", tz = "GMT")
        myTemp_data_L0 <- data.frame(mydata_4D$IT_ID, as.POSIXct(mydata_4D$Timestamp,
            origin = "1970-01-01"), mydata_4D$Tair/10)
        colnames(myTemp_data_L0) <- c("series", "ts", "value")
        myTemp_data_L0 <- myTemp_data_L0 \%>\% dplyr::filter(series ==
            ID[j])
        myTemp_data_L1 <- proc_L1(data_L0 = myTemp_data_L0, reso = 240,
            input = "long", date_format = "\%Y-\%m-\%d \%H:\%M:\%S",
            year = "asis", tz = "GMT")
        par(mfrow = c(1, 1))
        par(mar = c(5, 5, 5, 5))
        myDendro_data_L2 <- proc_dendro_L2(dendro_L1 = myDendro_data_L1,
            temp_L1 = myTemp_data_L1, interpol = 12, plot = TRUE,
            plot_export = TRUE, plot_name = ID[j], tz = "GMT")
    }
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
