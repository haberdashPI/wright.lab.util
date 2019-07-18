find.file.deltas <- function(dir,data,reversal.warning=TRUE,...){
  data$thresh = NA
  map.files(dir,data,function(data,row,file){
    file.data = load.trials(file)
    data[row,]$thresh =
      find.delta(file.data,reversal.warning=reversal.warning,filename=file,...)

    data
  })
}
