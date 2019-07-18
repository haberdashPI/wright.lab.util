require(plyr)
pretest.adjust <- function(df,x,y,by=NULL,pre.by,prefix='adj.',pretest='pretest',pretest.index=0){
  df.pre = ddply(df,pre.by,function(df){
    if(!any(df[,x]==pretest.index)) 
      df[,pretest] = NA
    else
      df[,pretest] = mean(df[df[,x]==pretest.index,y])
    df
  })
  
  if(length(by) > 0){
    ddply(df.pre,by,function(df){ covariate.adjust(df,x,y,pretest,prefix) })
  }else{
    covariate.adjust(df.pre,x,y,pretest,prefix)
  }
}
