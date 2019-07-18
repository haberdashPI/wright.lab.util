chunk.blocks <- function(data,chunk.size,
                         condition = 'condition', day = 'day',
                         block = 'block',day.chunk = 'day.block',chunk = 'chunk'){
  maximums = droplevels(aggregate(data[,block],
    by=list(condition = data[,condition],day = data[,day]),max))

  conditions = as.character(unique(maximums$condition))
  num.days = length(unique(maximums$day))

  block.max = array(dim=c(length(conditions),num.days))
  
  index = 0
  for(cond in conditions){
    index = index + 1
    
    days = rep(0,num.days)
    d = subset(maximums,condition == cond)
    days[d$day + 1] = ceiling(d$x / chunk.size)
    
    block.max[index,] = c(0,cumsum(days)[1:(num.days-1)])
  }

  rownames(block.max) = conditions

  new.data = data
  new.data[,day.chunk] = floor(new.data[,block] / chunk.size) +
    diag(block.max[as.character(new.data[,condition]),new.data[,day]+1])
  new.data[,chunk] = floor(new.data[,block] / chunk.size)
  
  list(data = new.data,block.max = block.max)
}
