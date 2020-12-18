## converts pvalue into value of color
.pvalue_color_conversion <- function(p_value, point_estimate, null_value_zero, lower_better){
  # (L)OR12 at row 1 column 2 represents
  # the odds in the row defining treatment (1) over the odds in the column defining treatment (2)
  # after conversion, p_color = 1 means p<0.0001 and OR favors the row defining treatment
  # p_color = -1 means p<0.0001 and OR favors the column defining treatment
  # lower_better = 1 (TRUE) means if OR12 < 1 <=> OR favors the row defining treatment (1)
  # lower_better = 0 (FALSE) means if OR12 < 1 <=> OR favors the column defining treatment (2)
  # null_value_zero = 1 (TRUE) means results are log odds ratio, risk difference, mean difference
  # null_value_zero = 0 (FALSE) means results are odds ratio, risk ratio
  if(is.na(p_value)){
    return(NA)
  }else{
    if(null_value_zero == 1){
      if((point_estimate <= 0 & lower_better == 1) | (point_estimate>0 & lower_better == 0)){.costant1 = 1}
      else{.costant1 = -1}
    }else{
      if((point_estimate <= 1 & lower_better == 1) | (point_estimate>1 & lower_better == 0)){.costant1 = 1}
      else{.costant1 = -1}
    }
    .costant2 <- log(1 - log(0.0001, base = 1.5), base = 1.5)
    return(log(1 - log(ifelse(p_value < 0.0001, 0.0001, p_value), base = 1.5), base = 1.5) / .costant2 * .costant1)
  }
}
