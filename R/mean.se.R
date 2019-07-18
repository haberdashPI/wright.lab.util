mean.se <- function(formula,data,error.col=NULL,error.lwd=NULL,
                    error.lty=1,error.length=0.05,draw=TRUE,...){
  args = list(...)  
  if(hasArg(lwd) && length(error.lwd) == 0)
    error.lwd = args$lwd
  else if(length(error.lwd) == 0)
    error.lwd = 1
    
  if(hasArg(col) && length(error.col) == 0)
    error.col = args$col
  else if(length(error.col) == 0)
    error.col = 'black'

  means = aggregate(formula,data,mean)
  ses = aggregate(formula,data,std.error)

  last = ncol(means)

  #browser()

  if(draw){
    arrows(ses[,1],means[,last] - ses[,last],
           ses[,1],means[,last] + ses[,last],error.length,
           angle=90,code=3,col = error.col,lty = error.lty,
           lwd = error.lwd)
    
    lines(means[,1],means[,last],...)
  }

  range(means[,last] - ses[,last],means[,last] + ses[,last],error.length)
}
