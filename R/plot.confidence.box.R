plot.confidence.box <- function(data,x,width,col="lightgrey"){
  e <- t.conf(data)
  m <- mean(data)

  polygon(x=c(x,x+width,x+width,x),y=c(m + e,m + e,m - e,m - e),
          col=col)
}
