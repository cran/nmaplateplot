## converts estimates into size of circle
.pe_size_conversion <- function(estimate, point_estimate, null_value_zero){
  # (L)OR12 at row 1 column 2 represents
  # the odds in the row defining treatment (1) over the odds in the column defining treatment (2)
  # null_value_zero = 1 (TRUE) means results are log odds ratio, risk difference, mean difference
  # null_value_zero = 0 (FALSE) means results are odds ratio, risk ratio
  if(is.na(estimate)){
    return(0)
  }else{
    if(null_value_zero == 1){
      if(point_estimate <= 0 ){return(ifelse(estimate < 0, abs(estimate), 0))}
      else{return(ifelse(estimate > 0, abs(estimate), 0))}
    }else{
      if(point_estimate <= 1){return(ifelse(estimate < 1, 1 / estimate, 0))}
      else{return(ifelse(estimate > 1, estimate, 0))}
    }
  }
}
