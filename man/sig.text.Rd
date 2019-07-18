\name{sig.text}
\alias{sig.text}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print text in color according to significance.
}
\description{
TODO
}
\usage{
sig.text(x, y = NULL, labels = seq_along(x), p.vals, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{p.vals}{
%%     ~~Describe \code{p.vals} here~~
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
function (x, y = NULL, labels = seq_along(x), p.vals, ...) 
{
    args = list(...)
    args$x = x
    args$y = y
    args$labels = labels
    args$font = sapply(p.vals, function(p.val) {
        if (p.val < 0.05) {
            2
        }
        else if (p.val < 0.1) {
            3
        }
        else {
            1
        }
    })
    args$col = sapply(p.vals, function(p.val) {
        if (p.val < 0.05) {
            "red"
        }
        else if (p.val < 0.1) {
            "forestgreen"
        }
        else {
            "black"
        }
    })
    do.call("text", args)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
