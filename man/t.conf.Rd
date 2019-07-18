\name{deviate}
\alias{deviate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Deviate points so they can be seen while plotted.
}
\description{
TODO
}
\usage{
t.conf(xs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xs}{
%%     ~~Describe \code{xs} here~~
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
function (xs) 
{
    qt(0.975, df = length(xs) - 1) * sd(xs)/sqrt(length(xs))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
