category.plot <- function(formula,data=NULL,category.names=NULL,
                          label.width=20,control=NULL,
                          confidence.start=0.0,
                          confidence.end=1.08,                          
                          control.column = 1,
                          label.height=0,
                          show.individual.data=TRUE,
                          label.categories=NULL,
                          type=c('points','hist'),
                          height.scale=1,
                          arrow.length=0.05,
                          hist.breaks='Sturges',
                          new.plot=TRUE,
                          x.offset=0,
                          p.values=NULL,...){
  
  dat = model.frame(formula,data)
  stopifnot(ncol(dat) >= 2)
  categories <- sprintf('category%d',1:(ncol(dat)-1))  
  colnames(dat) <- c('y',categories)

  for(col in categories){
    stopifnot(is.factor(dat[,col]))
    dat[,col] = dat[,col][,drop=TRUE]
  }

  num.categories <-
    prod(unlist(lapply(lapply(dat[,2:ncol(dat)],levels),length)))

  dat$x = 0
  cat.factor = 1
  #n = 1
  for(col in categories){
    cat.factor = cat.factor * (length(levels(dat[,col])) +
      if(cat.factor == 1) -0.75 else 1)
    dat$x = dat$x + (as.numeric(dat[,col]) -
      if(show.individual.data) 1 else 0.75) / cat.factor
  }
  #dat$x = dat$x * n;

  if(length(categories) > 1){
    means <- aggregate(dat[,c('x','y')],by=as.list(dat[,categories]),mean)
    ses <- aggregate(dat$y,by=as.list(dat[,categories]),std.error)
  }else{
    means <- aggregate(dat[,c('x','y')],by=list(dat[,categories]),mean)
    ses <- aggregate(dat$y,by=list(dat[,categories]),std.error)
    names(means)[1] = categories[1]
    names(ses)[1] = categories[1]  
  }
  names(ses)[names(ses) == 'x'] = 'se'
  means = merge(ses,means)
  means = means[order(means$x),]
  
  if(length(label.categories)==0)
    label.categories=rep(TRUE,length(categories))
  n.offsets = sum(label.categories)

  args = list(...)
  saved.xlab = all.vars(formula)[2]
  if(!hasArg(ylim)) args$ylim <- range(dat$y)
  if(!hasArg(xlim))
    args$xlim <- c(0,1.15)
  if(hasArg(xlab)) saved.xlab = args$xlab
  if(!hasArg(ylab)) args$ylab = all.vars(formula)[1]
  if(!hasArg(mar)) args$mar = c(5+3*(n.offsets-1),4,4,2)  
  if(hasArg(x.mgp)){
    saved.x.mgp = args$x.mgp    
    args$x.mgp <- NULL
  }else saved.x.mgp <- par()$mgp
  
  args$x <- means$x
  args$y <- means$y
  args$xlab = ''
  args$xaxt <- 'n'
  args$type <- 'n'
  if(new.plot) do.call("plot",args)

  if(length(control) > 0){
    plot.confidence.box(dat[as.character(dat[,1+control.column]) ==
                            control,]$y,confidence.start,confidence.end)
  }

  means$x = means$x + x.offset

  if(type[1] == 'points'){
    # draw standard errors
    arrows(means$x,means$y + means$se,means$x,means$y - means$se,
           angle=90,code=3,length=arrow.length,
           col=if(hasArg(col)) args$col else 'black')
    
    # draw means
    points(means$x,means$y,
           cex=2*(if(hasArg(cex)) args$cex else 1),
           pch=if(hasArg(pch)) args$pch else 22,
           col=if(hasArg(col)) args$col else 'black',
           bg=if(hasArg(bg)) args$bg else 'black')

    
    #browser()

    # create a value 'deviate' for each data point. Deviate's absolute
    # value is proportional to the minum distance between this points
    # x and any other x in the data set. Deviate's sign is determined
    # by the sign of the minimum distance. (So that for two data
    # points x and y, who are closest to each other out of all other
    # data points, x's sign will be different that y's sign).
    
    dat$deviate = 0
    
    for(i in 1:nrow(means)){
      same.categories = Reduce(function(x,y){x & y},
        lapply(categories,function(cat){ means[i,cat] == dat[,cat] }))

      # order indices by threshold, and then reverse order of even
      # indices. This makes the closest points, furthers from each
      # other in the given order.
      indices = which(same.categories)
      indices = indices[order(dat[indices,]$y)]
      indices =
        c(indices[seq(1,length(indices),2)],
          rev(indices[seq(2,length(indices),2)]))

      dat[indices,]$deviate =
        seq(-0.5,0.5,length.out=nrow(dat[indices,])) +
          rep(c(-1,-1/3,1/3,1),length.out=nrow(dat[indices,]))
    }
    
    # plot the data points, adjusting the x position by a random value
    # whose magnitude is mediated by the value 'deviate'. Thus points
    # which are closer to each other should end up placed at some
    # random x location which is hopefully far from its neighbors.
    # points not near any other points will be placed closed to the
    # center area where data points are placed.
    width = 1/cat.factor
    if(show.individual.data){
      points(dat$x + x.offset + width/2 +
             0.1*width *
             dat$deviate,
             dat$y,
             pch=if(hasArg(pt.pch)) args$pt.pch else 23,
             cex=0.5,
             col=if(hasArg(pt.col)) args$pt.pch else "black",
             bg=if(hasArg(pt.bg)) args$pt.bg else "black")
    }

    if(length(p.values) > 0){
      sig.points.x <- c()
      sig.points.y <- c()
      near.sig.points.x <- c()
      near.sig.points.y <- c()

      condition.index = 1
      for(p.value in p.values){
        if(p.value < 0.05){
          sig.points.x = c(sig.points.x,means$x[condition.index])
          if(!any(means[,control.column] == control) ||
             means$y[condition.index] <
             means[means[,control.column] == control,]$y)
            sig.points.y = c(sig.points.y,
              means$y[condition.index] - means$se[condition.index]*2)
          else
            sig.points.y = c(sig.points.y,
              means$y[condition.index] + means$se[condition.index]*2)

        }
        else if(p.value < 0.06){
          near.sig.points.x = c(near.sig.points.x,means$x[condition.index])
          if(!any(means[,control.column] == control) ||
             means$y[condition.index] <
             means[means[,control.column] == control,]$y)
            near.sig.points.y = c(near.sig.points.y,
              means$y[condition.index] - means$se[condition.index]*2)
          else
            near.sig.points.y = c(near.sig.points.y,
              means$y[condition.index] + means$se[condition.index]*2)
        }
        condition.index = condition.index + 1
      }

      points(sig.points.x,sig.points.y,pch='*',cex=2.0,
             col='blue',bg='blue')
      points(near.sig.points.x,near.sig.points.y,pch=16,cex=0.8,
             col='blue',bg='blue')
    }
  }
  if(type[1] == 'hist'){
    if(length(hist.breaks) == 1 && typeof(hist.breaks) == 'double')
      hist.breaks = pretty(dat$y,hist.breaks)

    # draw vertical histogram for each category
    for(row in 1:nrow(means)){
      same.categories = Reduce(function(x,y){x & y},
        lapply(categories,function(cat){ means[row,cat] == dat[,cat] }))

      heights <- hist(dat[same.categories,]$y,
                      breaks=hist.breaks,
                      plot=FALSE)
      heights$scaled = heights$density / max(heights$density)

      for(i in 1:length(heights$counts)){
        x = means$x[row]
        width = 1/cat.factor
        
        polygon(x=c(x,x+heights$scaled[i]*width*height.scale,
                    x+heights$scaled[i]*width*height.scale,x),
                y=c(heights$breaks[i],
                    heights$breaks[i],
                    heights$breaks[i+1],
                    heights$breaks[i+1]),
                col='gray',border='black')
      }
    }
  }

  col.labels = list()
  index = 0
  for(col in categories){
    index = index + 1
    
    cats = levels(dat[,col])
    col.labels[[col]] =
      if(label.categories[index] &&

         length(category.names[[index]]) > 0){
        break.labels(category.names[[index]][cats],label.width)
      }else{
        temp <- cats
        names(temp) <- cats
        temp
      }
  }
  cat.factor = 1
  index = -1
  all.ats = rep(0,nrow(means))
  n.repeat = 1
  n.cats = length(categories)
  offset = -1
  for(col in categories){
    index = index + 1
    n.levels = length(levels(means[,col]))
    cat.factor = cat.factor * (n.levels + if(cat.factor == 1) -0.75 else 1)
    all.ats = all.ats + (as.numeric(means[,col])-1) / cat.factor

    if(label.categories[index+1]){
      ats = sort(unique(all.ats) +
        if(index+1 < n.cats)
        0.25/cat.factor
        else 0.25/cat.factor)

      axis(1,
           at = ats,
           labels = rep(col.labels[[col]][levels(dat[,col])],n.repeat),
           mgp=c(0,-0.5+label.height*(n.offsets-index),
             -1+label.height*(n.offsets-index)),
           tick=FALSE,lty=0,padj=1)
    }
    n.repeat = n.repeat * n.levels    
  }
  means
}
