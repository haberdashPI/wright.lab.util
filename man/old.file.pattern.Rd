\name{old.file.pattern}
\alias{old.file.pattern}
\docType{data}
\title{
File patterns for older file patterns
}
\description{
TODO
}
\usage{data(old.file.pattern)}
\format{
  The format is:
List of 4
 $ pattern: chr "(^[A-Za-z]{2})([0-9]+[A-Za-z])([OND0-9]+).d[0-9]{2,3}$"
 $ day    : chr "\\3"
 $ subject: chr "\\1"
 $ task   : chr "\\2"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(old.file.pattern)
## maybe str(old.file.pattern) ; plot(old.file.pattern) ...
}
\keyword{datasets}
