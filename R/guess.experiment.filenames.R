guess.experiment.filenames <- function(dir,file.pattern=old.file.pattern,
                                       ignore.files='a^',
                                       default.month.codes = month.codes,
                                       subject.month.codes = list(),
                                       label.post.test = NULL,
                                       verified.ids = NULL){
  
  files = list.files(dir)
  files = files[!grepl(ignore.files,files)]

  organized.files = NULL
  
  subjects =
    extract.from.files(dir,files,file.pattern,'subject')

  if(length(files) == 0)
    stop('No files found in directory "',dir,'".',sep='')

  for(subject in unique(na.omit(subjects))){
    # the which function is here so that NA's are ignored
    subject.files = files[which(subjects == subject)]

    #browser()
    days = extract.from.files(dir,subject.files,file.pattern,'day')
    
    if(length(subject.month.codes[[subject]]) == 0)
      sorted = sort.day.strs(unique(days),default.month.codes)
    else
      sorted = sort.day.strs(unique(days),subject.month.codes[[subject]])
      
    day.index = -1
    if(!(subject %in% verified.ids)){
      warning(paste('Please verify that for ',subject,
                    ' the following order of days is correct: ',
                    paste(sorted,collapse=', '),sep=''))
    }

    for(day in sorted){
      day.index = day.index + 1
      day.files = subject.files[days == day]

      tasks = extract.from.files(dir,day.files,file.pattern,'task')
      for(task in unique(tasks)){
        task.files = day.files[tasks == task]

        new.organized.files =
          data.frame(filename = task.files,task = task,day = day.index,
                     id = subject,block = 1:length(task.files))
        if(length(organized.files) == 0){
          organized.files = new.organized.files
        }else{
          organized.files = rbind(organized.files,new.organized.files)
        }
      }
      
    }
    # label the post test if requested.
    if(length(label.post.test) > 0){
      organized.files[organized.files$id == subject &
                      organized.files$day == day.index,]$day =
                        label.post.test
    }
  }

  organized.files
}
           

