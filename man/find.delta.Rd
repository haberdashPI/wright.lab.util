\name{find.delta}
\alias{find.delta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find delta from data.
}
\description{
TODO
}
\usage{
find.delta(data, delta.column = "(SigLev|[[:alpha:]]+Diff|Delta)", filename = "UNKONWN", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{delta.column}{
%%     ~~Describe \code{delta.column} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
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
function (data, delta.column = "(SigLev|[[:alpha:]]+Diff|Delta)", 
    filename = "UNKONWN", ...) 
{
    col = grep(delta.column, colnames(data))
    if (length(col) == 0) {
        stop(paste("Could not find column pattern '", delta.column, 
            "' in file '", filename, "'.", sep = ""))
    }
    revs = reversals(data[, col[1]])
    n = length(revs)
    if (n < 7) {
        warning(paste("Insufficinet reversals to calculate threshold", 
            " for file \"", filename, "\".", sep = ""))
        NA
    }
    else if (n\%\%2 == 0) 
        mean(revs[4:n])
    else mean(revs[3:n])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
