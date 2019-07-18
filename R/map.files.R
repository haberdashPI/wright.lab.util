map.files <- function(dir,data,FUN){
  loading.seq = c('|','/','-','\\')#c('.','..','...','....')
  i = 0
  loading.message = paste('Loading: ',loading.seq[i + 1],sep='')
  cat(loading.message)
  for(row in 1:nrow(data)){
    i = (row / 50) %% length(loading.seq)

    cat('\r')
    #cat(paste(rep('\b',nchar(loading.message)),collapse=''))
    loading.message = paste('Loading: ',loading.seq[i + 1],sep='')
    cat(loading.message)
        
    #message(paste("Loading data from file '",data[row,]$filename,"'.",
    #              sep=''))
    data = FUN(data,row,paste(dir,data[row,]$filename,sep='/'))
  }
  cat('\n')

  data
}
