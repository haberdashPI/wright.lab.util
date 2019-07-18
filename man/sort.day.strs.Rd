\name{sort.day.strs}
\alias{sort.day.strs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sort days according to month codes.
}
\description{
TODO
}
\usage{
sort.day.strs(days, month.codes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{days}{
%%     ~~Describe \code{days} here~~
}
  \item{month.codes}{
%%     ~~Describe \code{month.codes} here~~
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
function (days, month.codes) 
{
    sortable.days = days
    for (row in 1:nrow(month.codes)) {
        sortable.days = sub(month.codes[row, 1], month.codes[row, 
            2], sortable.days)
    }
    days[order(sortable.days)]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
