\name{chunk.blocks}
\alias{chunk.blocks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Organize Data in Chunks
}
\description{
TODO
}
\usage{
chunk.blocks(data, chunk.size, condition = "condition", day = "day", block = "block", day.chunk = "day.block", chunk = "chunk")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{chunk.size}{
%%     ~~Describe \code{chunk.size} here~~
}
  \item{condition}{
%%     ~~Describe \code{condition} here~~
}
  \item{day}{
%%     ~~Describe \code{day} here~~
}
  \item{block}{
%%     ~~Describe \code{block} here~~
}
  \item{day.chunk}{
%%     ~~Describe \code{day.chunk} here~~
}
  \item{chunk}{
%%     ~~Describe \code{chunk} here~~
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
function (data, chunk.size, condition = "condition", day = "day", 
    block = "block", day.chunk = "day.block", chunk = "chunk") 
{
    maximums = droplevels(aggregate(data[, block], by = list(condition = data[, 
        condition], day = data[, day]), max))
    conditions = as.character(unique(maximums$condition))
    num.days = length(unique(maximums$day))
    block.max = array(dim = c(length(conditions), num.days))
    index = 0
    for (cond in conditions) {
        index = index + 1
        days = rep(0, num.days)
        d = subset(maximums, condition == cond)
        days[d$day + 1] = ceiling(d$x/chunk.size)
        block.max[index, ] = c(0, cumsum(days)[1:(num.days - 
            1)])
    }
    rownames(block.max) = conditions
    new.data = data
    new.data[, day.chunk] = floor(new.data[, block]/chunk.size) + 
        diag(block.max[as.character(new.data[, condition]), new.data[, 
            day] + 1])
    new.data[, chunk] = floor(new.data[, block]/chunk.size)
    list(data = new.data, block.max = block.max)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
