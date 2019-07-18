\name{load.trials}
\alias{load.trials}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load all trials from a file.
}
\description{
TODO
}
\usage{
load.trials(filename, header.pattern = "^(Try|Trial).*", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{header.pattern}{
%%     ~~Describe \code{header.pattern} here~~
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
function (filename, header.pattern = "^(Try|Trial).*", ...) 
{
    args = list(...)
    stream = file(filename)
    open(stream)
    line = readLines(stream, n = 1, warn = FALSE, ok = TRUE)
    while (length(line) > 0 && !grepl(header.pattern, line)) {
        line = readLines(stream, n = 1, warn = FALSE, ok = TRUE)
    }
    if (length(line) == 0) {
        stop(paste("Could not find header pattern  '", header.pattern, 
            "' in file '", filename, "'.", sep = ""))
    }
    columns = unlist(strsplit(line, if (length(args$sep) == 0 || 
        args$sep == "") 
        "[[:space:]]"
    else args$sep))
    data = tryCatch({
        read.table(stream, ...)
    }, error = function(e) {
        d = data.frame(t(1:length(columns)))[0, ]
    })
    close(stream)
    colnames(data) = columns
    data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
