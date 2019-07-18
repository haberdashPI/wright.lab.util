\name{new.file.pattern}
\alias{new.file.pattern}
\docType{data}
\title{
File patterns for newer data files.
}
\description{
TODO
}
\usage{data(new.file.pattern)}
\format{
  The format is:
List of 4
 $ pattern: chr "(^[0-9]{3,4})([0-9]{3}[DF])([0-9]{2,4}).d[0-9]{2}$"
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
data(new.file.pattern)
## maybe str(new.file.pattern) ; plot(new.file.pattern) ...
}
\keyword{datasets}
