find.block.outliers <- function(formula,data,mads.across = 'id',cutoff=3){
  
  fdata = model.frame(formula,data)
  
  dterms = terms(formula)
  response =
    as.character(attr(dterms,'variables')[[attr(dterms,'response')+1]])
  dependants =
    as.character(attr(dterms,'variables'))[-c(1,attr(dterms,'response')+1)]

  medians = aggregate(formula,fdata,median)
 
  names(medians) = sub(response,'block.median',names(medians),fixed=TRUE)

  across.d = fdata[,c(response,dependants)]
  across.d = fdata[,!(colnames(across.d) %in% mads.across)]
  
  mads = aggregate(as.formula(across.d),across.d,mad)
  colnames(mads) = sub(response,'block.mad',colnames(mads),fixed=TRUE)

  old.data = data
  data = merge(data,medians,all=TRUE,order=FALSE)
  data = merge(data,mads,all=TRUE,order=FALSE)
  
  if(nrow(old.data) != sum(!is.na(data$block.median))){
    stop('Missing rows when calculating median performance.')
  }

  #browser()
  
  data$outlier =
    data[,response] > data$block.median + cutoff*data$block.mad |
    data[,response] < data$block.median - cutoff*data$block.mad

  data
}
