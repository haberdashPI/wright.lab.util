load.trials <- function(filename,header.pattern = '^(Try|Trial).*',...){

  # TODO: this could be faster if we
  # checked for the '---------' line manually
  # and then read into a data frame.
  
  # Load trials from a block of training, skippin information in the header.
  #
  # Args:
  #  filename: the name of the file to load.
  #  header.pattern: a regular expression that matches
  #    the last line of the header.
  #
  # NOTES: The last line of the header is assumed to have
  # the names of the data columns in it.
  args = list(...)
  
  stream = file(filename);
  open(stream);
  line = readLines(stream,n=1,warn=FALSE,ok=TRUE);
  while(length(line) > 0 && !grepl(header.pattern,line)){
    line = readLines(stream,n=1,warn=FALSE,ok=TRUE)
  }

  if(length(line) == 0){
    stop(paste("Could not find header pattern  '",header.pattern,
               "' in file '",filename,"'.",sep = ''))
  }

  splitter =
    if(length(args$sep) == 0 || args$sep == '') '[[:space:]]+'
    else args$sep
  columns = unlist(strsplit(line,splitter))

  # ignore the first row of data if it is a string of "-------------"
  # or all whitespace
  first.row = readLines(stream,n=1,warn=FALSE,ok=TRUE)
  while(length(first.row) == 0 || grepl('^[[:space:]]*$',first.row) || grepl('^-+$',first.row)){
    first.row = readLines(stream,n=1,warn=FALSE,ok=TRUE)
  }
  vals = as.numeric(unlist(strsplit(first.row,splitter)))
  vals = vals[!is.na(vals)]
  first.row.data = data.frame(t(vals))
  # print(first.row.data)
  names(first.row.data) = columns

  # load all remaining rows
  data = tryCatch({read.table(stream,comment.char='',...)},
    error = function(e){
      if(e[[1]] == 'no lines available in input'){
        d = data.frame(t(1:length(columns)))[0,]
      }else{
        stop(e)
      }
    })
           
  close(stream)
  colnames(data) = columns

  rbind(first.row.data,data)
}
#data = load.reversals('../../data/f.50ms.1k.15b/1546101F122.d15',header.pattern='Trial')
