\name{extract.from.files}
\alias{extract.from.files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract information from files.
}
\description{
TODO
}
\usage{
extract.from.files(filenames, format, what)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filenames}{
%%     ~~Describe \code{filenames} here~~
}
  \item{format}{
%%     ~~Describe \code{format} here~~
}
  \item{what}{
%%     ~~Describe \code{what} here~~
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
function (filenames, format, what) 
{
    pattern = format$pattern
    replace = format[[what]]
    good.files = grepl(pattern, filenames)
    for (bad.file in filenames[!good.files]) {
        warning(paste("File \"", bad.file, "\" does not appear to follow the correct", 
            " filenaming convention.", sep = ""))
    }
    result = rep(NA, length(filenames))
    if (what == "day") {
        if (is.null(format$day)) {
            result[good.files] = strptime(file.info(filenaems[good.files])$mtime, 
                "\%Y-\%m-\%d")
        }
        else {
            result[good.files] = sub(pattern, replace, filenames[good.files])
        }
    }
    else result[good.files] = sub(pattern, replace, filenames[good.files])
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
