.from_rc_to_ullr <- function(Results, null_value_zero){
  # "row-column" type data: e.g. RR[i, j] represents risk of ith (treatment) over risk of jth (treatment)
  # "upperleft-lowerright" type data:
  # if i > j then RR[i, j] represents risk of jth (treatment) over risk of ith (treatment)
  # if i < j then RR[i, j] represents risk of ith (treatment) over risk of jth (treatment)
  # null_value_zero = 1 (TRUE) means the lower diagonal matix results are log odds ratio, risk difference, mean difference
  # null_value_zero = 0 (FALSE) means the lower diagonal matix results are odds ratio, risk ratio
  result_temp <- Results
  ntrt <- nrow(Results$Treatment_specific_values)
  if(null_value_zero == 1){
    for(ii in 2:ntrt){
      for(jj in 1:(ii-1)){
        result_temp$Point_estimates[ii,jj] <- -Results$Point_estimates[ii,jj]
        result_temp$Interval_estimates_LB[ii,jj] <- -Results$Interval_estimates_UB[ii,jj]
        result_temp$Interval_estimates_UB[ii,jj] <- -Results$Interval_estimates_LB[ii,jj]
      }
    }
  }else{
    for(ii in 2:ntrt){
      for(jj in 1:(ii-1)){
        result_temp$Point_estimates[ii,jj] <- 1/Results$Point_estimates[ii,jj]
        result_temp$Interval_estimates_LB[ii,jj] <- 1/Results$Interval_estimates_UB[ii,jj]
        result_temp$Interval_estimates_UB[ii,jj] <- 1/Results$Interval_estimates_LB[ii,jj]
      }
    }
  }
  return(result_temp)
}
