\name{mean.se}
\alias{mean.se}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the mean and standard error for some data.
}
\description{
TODO
}
\usage{
mean.se(formula, data, error.col = NULL, error.lwd = NULL, error.lty = 1, error.length = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{error.col}{
%%     ~~Describe \code{error.col} here~~
}
  \item{error.lwd}{
%%     ~~Describe \code{error.lwd} here~~
}
  \item{error.lty}{
%%     ~~Describe \code{error.lty} here~~
}
  \item{error.length}{
%%     ~~Describe \code{error.length} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (formula, data, error.col = NULL, error.lwd = NULL, 
    error.lty = 1, error.length = 0.05, ...) 
{
    args = list(...)
    if (hasArg(lwd) && length(error.lwd) == 0) 
        error.lwd = args$lwd
    else if (length(error.lwd) == 0) 
        error.lwd = 1
    if (hasArg(col) && length(error.col) == 0) 
        error.col = args$col
    else if (length(error.col) == 0) 
        error.col = "black"
    means = aggregate(formula, data, mean)
    ses = aggregate(formula, data, std.error)
    last = ncol(means)
    arrows(ses[, 1], means[, last] - ses[, last], ses[, 1], means[, 
        last] + ses[, last], error.length, angle = 90, code = 3, 
        col = error.col, lty = error.lty, lwd = error.lwd)
    lines(means[, 1], means[, last], ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
