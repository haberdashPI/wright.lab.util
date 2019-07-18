sort.day.strs <- function(days,month.codes){
  sortable.days = days
  for(row in 1:nrow(month.codes)){
    sortable.days = sub(month.codes[row,1],month.codes[row,2],sortable.days)
  }
  
  days[order(sortable.days)]
}
