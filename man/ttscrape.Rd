\name{ttscrape}
\alias{ttscrape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to do download and split ITTNet data
}
\description{
the function downloads ITTNet data from specific servers and split the different strings into specific data frames for further processing
}
\usage{
ttscrape(ID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ID}{
%%     ~~Describe \code{ID} here~~
the ID of yout ttCloud
}
  \item{subset_days}{
%%     ~~Describe \code{ID} here~~
days to be analyzed from the end of measurements (e.g. "today"). "all" doesn't subset the timeseries.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns data frames of type:
mydata_4B: general sensors of each treetalker connected to this ttcloud
mydata_49: treetalkers and ttR spectrometers' data
mydata_4D: ttcloud data
mydata_4C: not implemented
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
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
  ttscrape("C0200091", 10)
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
