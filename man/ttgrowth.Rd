\name{ttGrowth}
\alias{ttGrowth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to process and plot dendrometer data
}
\description{
the function makes use of data frames created by the "ttscrape" function. It is makes use of the "treenetproc" R-package for analyzing the Sharp data within a mesh of TreeTalkers.
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
ttscrape("C0200091")
ttGrowth(mydata_4D, "all_in_one")
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
