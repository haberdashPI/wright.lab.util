# Allows aggregation of statistics across multiple columns.
# FUN is called for each unique grouping of variables on the right
# hand side of the formula. The arguments to the function are the
# columns corresponding to the left hand side of the formula.
aggregate.m <- function(x,...) UseMethod("aggregate.m")

aggregate.m.data.frame <- function(data,args,by,FUN){
  index.column = ncol(data)+1
  data[,index.column] = 1:nrow(data)

  aggregate(1:nrow(data),by=by,
            function(indices){
              do.call(FUN,unname(as.list(as.data.frame(data[indices,args]))))
            })
}

aggregate.m.formula <- function(formula,data=list(),FUN){
  # setup data and add an indexing column
  data = model.frame(formula,data)

  columns = attr(terms(formula),"term.labels")
  if(length(columns) > 1)
    by.labels = as.list(data[,columns])
  else{
    by.labels = list()
    by.labels[[columns]] = data[,columns]
  }
    

  # call aggregate over the indexing column, and then use the indices 
  # to make single funciton call across all columns on the left hand
  # side of the formula.
  aggregate(1:nrow(data),by=by.labels,
            function(indices){
              do.call(FUN,unname(as.list(as.data.frame(data[indices,1]))))
              # note: the first column contains all columns
              # on the left hand side, by virtue of cbind.
            })
}
