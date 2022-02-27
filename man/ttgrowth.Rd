\name{ttGrowth}
\alias{ttGrowth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to process and plot TreeTalker radial growth data.
}
\description{
The function makes use of data frames created by the "ttScrape()" function. It processes data from GP2Y0A21 Sharp distance sensors. The sensors are deployed to measure stem radial growth. The distance sensor is positioned at few centimeters away from the tree trunk’s surface and kept in place by a carbon fiber stick anchored in the xylem. The "ttGrowth()" function converts the digital numbers into distance (mm) and applyes a temporal averaging (median) on weekly basis.
}
\usage{
ttGrowth(mydata_4D)
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
TT+ user manual, Nature 4.0 (version 02/09/2020).
}
\author{
%%  ~~who you are~~
Enrico Tomelleri
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ttScrape("C0200091")
ttGrowth(mydata_4D, "split")
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
