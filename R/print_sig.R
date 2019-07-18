print_sig = function(xs){
  ifelse(xs > 0.1,' ',
  ifelse(xs < 0.001,'***',
  ifelse(xs < 0.010,' **',
  ifelse(xs < 0.050,'  *',
  ifelse(xs < 0.100,'  .',
                    '   ')))))
}
