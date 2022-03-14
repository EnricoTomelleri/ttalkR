\name{ttOutput}
\alias{ttOutput}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to create a new SQLite database and associated structure.
)
}
\description{
The function makes use of data frames created by functions from the "ttalkR" package.
}
\usage{
ttOutput(ID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ID}{
%%     ~~Describe \code{mydata_4D} here~~
the ID of yout ttCloud
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
%ttScrape("C0200091", 10)
%ttBattery(mydata_4B, mydata_4D, "all_in_one")



load(file = "data/mydata_4D.rda")
load(file = "data/mydata_49.rda")
load(file = "data/mydata_4B.rda")

ttTair(mydata_4D, "split")
%load(file = "data/mydata_4D.rda")
ttStWC(mydata_4D, "spruce", "split")
%load(file = "data/mydata_4D.rda")
ttStability(mydata_4D, "split")
%load(file = "data/mydata_4D.rda")
ttRH(mydata_4D, "split")
%load(file = "data/mydata_49.rda")
ttLight(mydata_49, lat=46.45, lon=11.25, 860, "split")
%load(file = "data/mydata_4D.rda")
ttGrowth(mydata_4D, "split")
%load(file = "data/mydata_4D.rda")
ttGranier(mydata_4D, "split")
%load(file = "data/mydata_4B.rda")
%load(file = "data/mydata_4D.rda")
ttBattery(mydata_4B, mydata_4D, "all_in_one")

ttOutput("site_test")
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
