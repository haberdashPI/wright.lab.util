extract.from.files <- function(parent,filenames,format,what){
  pattern = format$pattern
  replace = format[[what]]

  good.files = grepl(pattern,filenames)
  for(bad.file in filenames[!good.files]){
    warning(paste('File "',bad.file,
                  '" does not appear to follow the correct',
                  ' filenaming convention.',sep=''))
  }

  result = rep(NA,length(filenames))

  if(what == 'day'){
    if(is.null(format$day)){
      result[good.files] =
        as.character(strptime(file.info(paste(parent,filenames[good.files],sep='/'))$mtime,
                 "%Y-%m-%d"))
    }else{
      result[good.files] = sub(pattern,replace,filenames[good.files])
    }
  }else result[good.files] = sub(pattern,replace,filenames[good.files])

  result
}
