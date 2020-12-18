## reorder the matrix
.reorder_matrix <- function(m, order_temp, null_value_zero_uDM, null_value_zero_lDM){
  ntrt <- nrow(m)
  m_lower <- matrix(0, ntrt, ntrt)
  if(null_value_zero_lDM == 1){
    m_lower[upper.tri(m_lower)] <- -t(m)[upper.tri(m)]
  }else if(null_value_zero_lDM == 0){
	  m_lower[upper.tri(m_lower)] <- 1/t(m)[upper.tri(m)]
  }else{
	  m_lower[upper.tri(m_lower)] <- t(m)[upper.tri(m)]
  }
  m_lower[lower.tri(m_lower)] <- m[lower.tri(m)]
  m_lower <- m_lower[order_temp, order_temp]

  m_upper <- matrix(0, ntrt, ntrt)
  if(null_value_zero_uDM == 1){
    m_upper[lower.tri(m_upper)] <- -t(m)[lower.tri(m)]
  }else if(null_value_zero_uDM == 0){
    m_upper[lower.tri(m_upper)] <- 1/t(m)[lower.tri(m)]
  }else{
    m_upper[lower.tri(m_upper)] <- t(m)[lower.tri(m)]
  }
  m_upper[upper.tri(m_upper)] <- m[upper.tri(m)]
  m_upper <- m_upper[order_temp, order_temp]

  m_new <- matrix(0, ntrt, ntrt)
  m_new[lower.tri(m_new)] <- m_lower[lower.tri(m_lower)]
  m_new[upper.tri(m_new)] <- m_upper[upper.tri(m_upper)]
  return(as.data.frame(m_new))
}
