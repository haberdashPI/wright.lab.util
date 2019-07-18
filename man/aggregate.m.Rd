\name{aggregate.m}
\alias{aggregate.m}
\title{
Aggregate from multiple columns.
}
\description{
Augments the aggregate function with the ability to handle functions
that require mulitple arguments. Each column specified in the response
is a different argument for FUN. 
}
\usage{
aggregate.m(formula, data, FUN)

aggregate.m(data,args,by,FUN)
}
\arguments{
  \item{formula}{
    The formula overwhich to aggregate. The response variable is given
    multiple columns by using cbind. e.g. cbind(x,y) ~ group
}
  \item{data}{
    The data to aggregate over.
  }
  \item{args}{
    The columns which are to be the input arguments to the aggregating
    funciton, in order.
  }
  \item{by}{
    The actual columns which data will be aggregated by, as per
    aggregate.
  }
  \item{FUN}{
    The function by which to aggregate. There should be the same number
    of arguments to this function as there are columns in the dependent
    variable of the formula or the column names in 'args'.
  }
}
\value{
  
An aggregate data frame, for each unique set of 'by' columns (or the
dependent variables of the fromula version is used) the result of apply
the function to all rows with those column values is given.

}
\author{
David Little
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
