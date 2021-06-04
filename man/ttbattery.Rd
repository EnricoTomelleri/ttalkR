\name{ttbattery}
\alias{ttbattery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a function to do check and plot the battery status of a TT site
}
\description{
the function makes use of data frames created by the "ttscrape" function and plots the battery voltage for all the TreeTalkes in a mesh, the TTR and the TTcloud.
}
\usage{
ttbattery(mydata_4B, mydata_4D, plot_label)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata_4B}{
%%     ~~Describe \code{mydata_4B} here~~
a ttscrape object
}
  \item{mydata_4D}{
%%     ~~Describe \code{mydata_4D} here~~
a ttscrape object
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
ttscrape("C0200091")
ttbattery(mydata_4B, mydata_4D, "all_in_one")
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
