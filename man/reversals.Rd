\name{reversals}
\alias{reversals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate reversals of adaptive track.
}
\description{
TODO
}
\usage{
reversals(deltas)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deltas}{
%%     ~~Describe \code{deltas} here~~
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
function (deltas) 
{
    n = length(deltas)
    delta.changes = deltas[2:n] - deltas[1:(n - 1)]
    non.zero.indices = which(abs(delta.changes) > 0)
    non.zero.changes = delta.changes[non.zero.indices]
    m = length(non.zero.changes)
    reversals = which((non.zero.changes[2:m] * non.zero.changes[1:(m - 
        1)]) < 0)
    deltas[non.zero.indices[reversals + 1] + 1]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
