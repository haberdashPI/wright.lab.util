mean.se.lines <- function(formula,data,error.col='lightgrey',error.lwd=NULL,
                          error.border='black',error.lty=NULL,...){
  args = list(...)

  if(hasArg(col) && length(error.border) == 0)
    error.border = args$col
  else if(length(error.border) == 0)
    error.border = 'black'

  if(hasArg(lty) && length(error.lty) == 0)
    error.lty = args$lty
  else if(length(error.lty) == 0)
    error.lty = 1

  if(hasArg(lwd) && length(error.lwd) == 0)
    error.lwd = 0.5 * args$lwd
  else if(length(error.lwd) == 0)
    error.lwd = 0.5

  means = aggregate(formula,data,mean)
  ses = aggregate(formula,data,std.error)

  last = ncol(means)

  stopifnot(!any(is.na(ses[,last])))  

  polygon(c(ses[,1],rev(ses[,1])),
          c(means[,last] - ses[,last],rev(means[,last] + ses[,last])),
          col = error.col,lwd = error.lwd,border = error.border,
          lty=error.lty)
  lines(means[,1],means[,last],...)
}
