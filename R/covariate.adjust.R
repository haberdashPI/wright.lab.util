require(plyr)
covariate.adjust <- function(df,x,y,covariate,prefix='adj.'){
  slope.name = paste(prefix,'slope.',y,sep='')
  slopes = ddply(df,x,function(df){
    df = data.frame(slope=
        coef(lm(reformulate(response = y, termlabels=covariate),df))[2])
    colnames(df) = slope.name
    df
  })

  df = merge(df,slopes)
  df[,paste('mean',covariate,sep='.')] = mean(df[,covariate],na.rm=TRUE)
  df[,paste(prefix,y,sep='')] = df[,y] - df[,slope.name] *
    (df[,covariate] - mean(df[,covariate],na.rm=TRUE))
  df
}

