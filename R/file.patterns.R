old.file.pattern =
  list(pattern='(^[A-Za-z]{2})([0-9]+[A-Za-z])[0-9]{2}.([OND0-9]+)$',
    day='\\3',subject='\\1',task='\\2')

new.file.pattern =
    list(pattern='(^[0-9]{3,4})([0-9]{3}[DF])([0-9]{2,4}).d[0-9]{2}$',
         day='\\3',subject='\\1',task='\\2')

# this pattern works correctly for most month codes but might have a
# problem with ambiguosly coded dates near the end of the year (if 10,
# 11 and 12 are used instead of O N and D).
month.codes =
  rbind(c("O([0-9]{2})$","10\\1"),
        c("N([0-9]{2})$","11\\1"),  
        c("D([0-9]{2})$","12\\1"),
        
        c("O([0-9]{1})$","100\\1"),
        c("N([0-9]{1})$","110\\1"),  
        c("D([0-9]{1})$","120\\1"),
        
        c("^([0-9]{3})$","0\\1"),
        c("^([0-9])([0-9])$","0\\10\\2"))

# if you're having trouble with ordering of dates later in the year
# use this set of codes
late.month.codes =
  rbind(c("O([0-9]{2})$","10\\1"),
        c("N([0-9]{2})$","11\\1"),  
        c("D([0-9]{2})$","12\\1"),
        
        c("O([0-9]{1})$","100\\1"),
        c("N([0-9]{1})$","110\\1"),  
        c("D([0-9]{1})$","120\\1"),
        
        c("^([0-1]{1}[0-9]{1})([0-9]{1})$","\\10\\2"),
        c("^([2-9]{1})([0-9]{2})$","0\\1\\2"),        
        c("^([0-9])([0-9])$","0\\10\\2"))

