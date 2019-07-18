find.delta <- function(data,...) UseMethod("find.delta")

find.delta.default <- function(x,filename='UNKNOWN',reversal.warning=TRUE,
                               find.sd=FALSE){
  revs = reversals(x)
  n = length(revs)
  final.revs = 
    if( n < 7 ){
      if(reversal.warning){
        warning(paste('Insufficinet reversals to calculate threshold',
                      ' for file "',filename,'".',sep=''))
      }
      
      NA
    }
    else if( n %% 2 == 0 ) revs[5:n]
    else revs[4:n]
  if(find.sd) list(mean=mean(final.revs),sd=sd(final.revs)) else mean(final.revs)
}

find.delta.data.frame <-
  function(data,delta.column='(SigLev|[[:alpha:]]+Diff|Delta)',filename,
           min.blocks=10,find.sd=FALSE,...){
  col = grep(delta.column,colnames(data))
 
  final.revs =
    if(length(col) == 0){
      stop(paste("Could not find column pattern '",delta.column,
                 "' in file '",filename,"'.",sep=''))
    }else if(nrow(data) < min.blocks){
      warning(paste('File "',filename,'" has only ',nrow(data),
                    " row(s). The file will be ignored.",sep=''))
      if(find.sd) list(mean=NA,sd=NA) else NA
    }else{
      find.delta.default(data[,col[1]],filename,find.sd,...)
    }
}
