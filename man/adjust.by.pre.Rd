\name{adjust.by.pre}
\alias{adjust.by.pre}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adjusting Learning Threshold
}
\description{
  
Used for adjusting learning thresholds by a pre-test. In learning
studies usually the amount of learning is influenced by where a
subject's performance began at the start of learning. To account for
this one can either introduce the pre test as a covariate in statistical
anlaysis, or more informally for graphs and the like, one can adjust the
performance measure so that it isas if all participants started with the
same pre-test, assuming a linear relationship between the pre-test and
any other test. This function returns a modified data frame that allows
both approaches to be easilly utilized, by adding column to every row
with the adjusted performance measure that row and the the pre test
performance.

}
\usage{
adjust.by.pre(formula,data=list(),agg.slope.by = NULL,agg.pre.by = NULL)
}
\arguments{
  \item{formula}{
    A formula describing the data frame. The dependent variable
    indicates a numeric performance measure to be adjusted. The first
    independent variable should be numeric, and is the indication of
    time (e.g. testing days). The value 0 is assumed to be the
    pre-test. All other variables should be ordinal or nominal grouping
    variables. Each unique combination of grouping variables is assumed
    to be a unique subject.
  }
  \item{data}{
    The data environment for the aforementioned formula.
  }
  \item{agg.slope.by}{
    
    By default (when this is NULL) the slope used to adjuste the
    pre-test to a given day is found across all conditions. If you would
    like to calculate the slope across some subset of the data, specify
    which columns the slope should be aggregated by using this variable.
    
  }
  \item{agg.pre.by}{
    When adjusting pre-tests there must be a mean pre-test to adjust
  to. This is normally done across the entire data set (when agg.pre.by
  is NULL). If you would like to calculate the mean pre-test across some
  subset of the data, specify which columsn the mean pre-test should be
  aggregated by using this variable.
  }
}
\value{

}
\references{
    Cohen, J. (1988). Statistical power analysis for the behavioral sciences. (2nd ed.). Hillsdale, NJ: Erlbaum.
}
\author{
David Little
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
