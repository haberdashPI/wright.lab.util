break.labels <- function(labels,label.width){
  sapply(labels,function(label){
    Reduce(function(x,y){paste(x,y,sep="\n")},
           strwrap(label,label.width))
  })
}
