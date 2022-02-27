\name{ttStWC}
\alias{ttStWC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to plot and process relative humidity data from the Capacitive sensor (MicroPCB (20x3x2) mm with copper plates).
}
\description{
the function makes use of data frames created by the "ttScrape" function.
}
\usage{
ttStWC(mydata_4D, species, plot_label)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata_4D}{
%%     ~~Describe \code{mydata_4D} here~~
a ttScrape object
}
  \item{plot_label}{
%%     ~~Describe \code{plot_label} here~~
"spruce", "beech", "poplar", "pine"
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
Asgharinia, S., Belelli Marchesini, L., Gianelle, D., and Valentini, R.: Design and Performance Evaluation of Internet of Things (IoT) Based Multifunctional Device for Plant Ecophysiology & Hydrology: Toward Stem Water Content & Sap Flow , EGU General Assembly 2020, Online, 4–8 May 2020, EGU2020-17021, https://doi.org/10.5194/egusphere-egu2020-17021, 2020
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
ttStWC(mydata_4D, "beech", "all_in_one")
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
