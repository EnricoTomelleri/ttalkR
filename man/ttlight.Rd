\name{ttLight}
\alias{ttLight}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to process and plot TreeTalker spectrometer data.
}
\description{
The function makes use of data frames created by the "ttScrape()" function.  It processes data from AMS chips, Model: AS7262 (Visible range), AS7263 (Near Infra-Red range) sensors. It filters, aggregate to daily values and plot spectrometer data from a TreeTalkers mesh. The filtering is based on solar geometry base on provided coordinated (lat, lon). The function doesn't account for topography. The filtered data include measurements with the sun position from -30 to + 30 degrees from the geographic South.
}
\usage{
ttLight(mydata_49, lat, lon, wavelength, plot_label)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata_49}{
%%     ~~Describe \code{mydata_49} here~~
a ttScrape object
}
  \item{lat}{
%%     ~~Describe \code{plot_label} here~~
central latitude of the measurement plot
}
  \item{lon}{
%%     ~~Describe \code{plot_label} here~~
central longitude of the measurement plot
}
  \item{wavelength}{
%%     ~~Describe \code{plot_label} here~~
it identifies the spectral band to be plotted. Options are: 450, 500, 550, 570, 600, 650, 610, 680, 730, 760, 810 and 860 nm
}
  \item{plot_label}{
%%     ~~Describe \code{plot_label} here~~
"all_in_one": all the treetalkers in a plot
"split": each treetalker in a different panel
"none": it doesn't plot
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
%ttScrape("C0200091")
load(file = "data/ttTestData.rda")
ttLight(mydata_49, lat=46.453, lon=11.232, 860, "split")
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
