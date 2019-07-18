\name{mean.se.lines}
\alias{mean.se.lines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the mean and standard error using lines.
}
\description{
TODO
}
\usage{
mean.se.lines(formula, data, error.col = "lightgrey", error.lwd = NULL, error.border = "black", error.lty = NULL, ...)
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
  \item{error.border}{
%%     ~~Describe \code{error.border} here~~
}
  \item{error.lty}{
%%     ~~Describe \code{error.lty} here~~
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
function (formula, data, error.col = "lightgrey", error.lwd = NULL, 
    error.border = "black", error.lty = NULL, ...) 
{
    args = list(...)
    if (hasArg(col) && length(error.border) == 0) 
        error.border = args$col
    else if (length(error.border) == 0) 
        error.border = "black"
    if (hasArg(lty) && length(error.lty) == 0) 
        error.lty = args$lty
    else if (length(error.lty) == 0) 
        error.lty = 1
    if (hasArg(lwd) && length(error.lwd) == 0) 
        error.lwd = 0.5 * args$lwd
    else if (length(error.lwd) == 0) 
        error.lwd = 0.5
    means = aggregate(formula, data, mean)
    ses = aggregate(formula, data, std.error)
    last = ncol(means)
    stopifnot(!any(is.na(ses[, last])))
    polygon(c(ses[, 1], rev(ses[, 1])), c(means[, last] - ses[, 
        last], rev(means[, last] + ses[, last])), col = error.col, 
        lwd = error.lwd, border = error.border, lty = error.lty)
    lines(means[, 1], means[, last], ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
