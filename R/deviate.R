deviate <- function(xs,ys){
  devs = c()
  for(y in sort(unique(ys))){
    dev = seq(-1,1,length.out=sum(ys==y)) + 
      3*rep(c(-1,1,-1/3,1/3),length.out=sum(ys==y))
    dev = dev / max(dev)
    
    devs = c(devs,dev)
  }
  dev.final = rep(0,length(xs))
  print(length(xs))
  print(length(devs))
  dev.final[order(xs,ys)] = devs

  dev.final
}
