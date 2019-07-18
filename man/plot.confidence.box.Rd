\name{plot.confidence.box}
\alias{plot.confidence.box}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a box for the confidence interval.
}
\description{
TODO
}
\usage{
plot.confidence.box(data, x, width, col = "lightgrey")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
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
function (data, x, width, col = "lightgrey") 
{
    e <- t.conf(data)
    m <- mean(data)
    polygon(x = c(x, x + width, x + width, x), y = c(m + e, m + 
        e, m - e, m - e), col = col)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
