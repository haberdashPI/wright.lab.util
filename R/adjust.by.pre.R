adjust.by.pre <- function(formula,data=list(),agg.slope.by = NULL,
                          agg.pre.by = NULL){

  d = model.frame(formula,data)
  labels = attr(terms(d),'term.labels')

  response.label =
    as.character(attr(terms(d),'variables')[[attr(terms(d),'response')+1]])
  indexing.label = labels[1]
  grouping.labels = labels[2:length(labels)]
  pre.label = paste(response.label,'pre',sep='.')
  adj.label = paste('adj',response.label,sep='.')
   
  classes = attr(terms(d),'dataClasses')
  if(classes[indexing.label] != 'numeric'){
    stop("adjust.by.pre expects the first dependent variable to be numeric.")
  }
  if(!all(classes[grouping.labels] != 'numeric')){
    stop(paste("adjust.by.pre expects all depednent variables other than",
               "the first to be grouping variables, i.e. non-numeric."))
  }
  if(!all(agg.slope.by %in% grouping.labels)){
    stop(paste('agg.slope.by included labels not presenting in',
               'formula grouping variables:',
               agg.slope.by[!(agg.slope.by %in% grouping.labels)]))
  }

  # add mean of pre test to every column
  means = aggregate(formula,data,mean)
 
  means = merge(means,
    means[means[,indexing.label] == 0,],by=grouping.labels,
    suffixes=c('.day','.pre'))
  names(means) = sub('.day','',names(means))

  # find slope of pre test to each day
  temp = means
  temp$x1 = means[,response.label]
  temp$x2 = means[,pre.label]

  by.list = list()
  by.list[[indexing.label]] = temp[,indexing.label]
  for(split in agg.slope.by)
    by.list[[split]] = temp[,split]
  slopes = aggregate.m(as.data.frame(temp),c('x1','x2'),by.list,
    function(post,pre){coef(lm(post ~ pre))[2]})
  
  names(slopes) = sub('^x$','slope',names(slopes))

  d = merge(data,
    means[means[,indexing.label] == 0,c(grouping.labels,response.label)],
    by=grouping.labels,
    suffixes=c('.day','.pre'))
  d = merge(d,slopes)
  names(d) = sub('.day','',names(d))

  # find mean of pre test
  temp = means
  temp = temp[temp[,indexing.label] == 0,]
  temp = temp[,c(response.label,agg.pre.by)]
  if(class(temp) == 'data.frame'){
    pre.mean = aggregate(as.formula(temp),temp,mean)
    names(pre.mean) = sub(response.label,'pre.mean',names(pre.mean))
    d = merge(d,pre.mean,by=agg.pre.by)
  }else{
    d$pre.mean = mean(temp)
  }

  d[,adj.label] = d[,response.label] - d$slope * (d[,pre.label] - d$pre.mean)
  d[d[,indexing.label] == 0,adj.label] = d[d[,indexing.label] == 0,]$pre.mean

  d
}
