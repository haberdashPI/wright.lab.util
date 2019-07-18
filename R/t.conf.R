require(stats)
t.conf <- function(xs){
  qt(0.975,df=length(xs)-1)*sd(xs)/sqrt(length(xs))
}
