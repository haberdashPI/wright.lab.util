sig.text <- function(x,y=NULL,labels=seq_along(x),p.vals,...){
  args = list(...)
  args$x = x
  args$y = y
  args$labels = labels
  
  args$font = sapply(p.vals,
    function(p.val){
      if(p.val < 0.05){2}
      else if(p.val < 0.1){3}
      else{1}
    })

  args$col = sapply(p.vals,
    function(p.val){
      if(p.val < 0.05){'red'}
      else if(p.val < 0.1){'forestgreen'}
      else{'black'}
    })

  do.call("text",args)
}
