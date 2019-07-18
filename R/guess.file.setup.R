require(plyr)
require(stringr)

guess.file.setup = function(dir,pattern,column.patterns){
    files = list.files(dir,pattern,full.names=TRUE)
    file_names = list.files(dir,pattern,full.names=FALSE)
    details = file.info(files)
    reorder = order(as.POSIXct(details$ctime))
    details = details[reorder,]
    files = files[reorder]
    file_names = file_names[reorder]

    files = rownames(details)

    data = NULL
    for(i in 1:length(files)){
        d = data.frame(file = files[i],
                       date = format(details[i,]$mtime,format='%Y_%m_%d'))

        for(pattern in names(column.patterns))
            d[,pattern] =
                regmatches(file_names[i],regexec(column.patterns[[pattern]],
                                                 file_names[i]))[[1]][2]

        data = rbind(data,d)
    }

    data
}
