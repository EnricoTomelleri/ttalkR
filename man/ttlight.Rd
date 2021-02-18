\name{ttlight}
\alias{ttlight}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to process and plot spectrometer data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ttlight(mydata_49, plot_label)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata_49}{
%%     ~~Describe \code{mydata_49} here~~
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
function (mydata_49, plot_label)
{
    mydata_49$factor[mydata_49$gain == 0] <- 1
    mydata_49$factor[mydata_49$gain == 1] <- 3.7
    mydata_49$factor[mydata_49$gain == 2] <- 16
    mydata_49$factor[mydata_49$gain == 3] <- 64
    AS7263_610_R <- -312.45 + (1.6699 * (as.numeric(mydata_49$AS7263_610) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7263_680_R <- -561.56 + (1.5199 * (as.numeric(mydata_49$AS7263_680) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7263_730_R <- -1511.2 + (1.6209 * (as.numeric(mydata_49$AS7263_730) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7263_760_R <- -1012.5 + (1.4549 * (as.numeric(mydata_49$AS7263_760) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7263_810_R <- 91.58 + (0.8414 * (as.numeric(mydata_49$AS7263_810) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7263_860_R <- 334.88 + (0.531 * (as.numeric(mydata_49$AS7263_860) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7262_450_R <- -212.62 + (0.4562 * (as.numeric(mydata_49$AS7262_450) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7262_500_R <- -232.13 + (0.6257 * (as.numeric(mydata_49$AS7262_500) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7262_550_R <- -842.1 + (1.0546 * (as.numeric(mydata_49$AS7262_550) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7262_570_R <- -666.72 + (1.0462 * (as.numeric(mydata_49$AS7262_570) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7262_600_R <- -328.08 + (0.8654 * (as.numeric(mydata_49$AS7262_600) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    AS7262_650_R <- 202.77 + (0.7829 * (as.numeric(mydata_49$AS7262_650) *
        2.8 * mydata_49$integration_T/mydata_49$factor))
    out <- data.frame(AS7262_450_R, AS7262_500_R, AS7262_550_R,
        AS7262_570_R, AS7262_600_R, AS7262_650_R, AS7263_610_R,
        AS7263_680_R, AS7263_730_R, AS7263_760_R, AS7263_810_R,
        AS7263_860_R, mydata_49$IT_ID)
    out[out < 0] <- NA
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
