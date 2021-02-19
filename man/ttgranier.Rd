\name{ttgranier}
\alias{ttgranier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to process and plot data from Thermal dissipation probes
}
\description{
the function makes use of data frames created by the "ttscrape" function. It converst voltages into temperatures, it smooths the time series with a Savysky-Golay filter and estimate the sapflow for each TreeTalker in a mesh.
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
Enrico Tomelleri}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ttgranier(mydata_4D, plot_label=T)
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
