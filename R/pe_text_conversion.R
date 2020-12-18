## generate the text in cells (the Point estimate with 95% credible Interval)
.pe_text_conversion_u <- function(point_estimate, lower_bound, upper_bound, p_value, symbol_indicator){
  if(is.na(p_value)){
    return(NA)
  }else{
    if(p_value < 0.05){
      temp_text <- paste("underline( ", point_estimate)
      if(substr(point_estimate, nchar(point_estimate), nchar(point_estimate)) == "0"){
        if(substr(point_estimate, nchar(point_estimate)-1, nchar(point_estimate)) == "00"){
          temp_text <- paste(temp_text, " *. *0 *0")
        }else{
          temp_text <- paste(temp_text, " *0")
        }
      }
      if(!is.na(symbol_indicator)){
        temp_text <- paste(temp_text, " ) ^ ", symbol_indicator)
      }else{
        temp_text <- paste(temp_text, " )")
      }
     }else{
      temp_text <- paste(point_estimate)
      if(substr(point_estimate, nchar(point_estimate), nchar(point_estimate)) == "0"){
        if(substr(point_estimate, nchar(point_estimate)-1, nchar(point_estimate)) == "00"){
          temp_text <- paste(temp_text, " *. *0 *0")
        }else{
          temp_text <- paste(temp_text, " *0")
        }
      }
      if(!is.na(symbol_indicator)){
        temp_text <- paste(temp_text, " ^ ", symbol_indicator)
      }
     }
    return(temp_text)
  }
}


.pe_text_conversion_l <- function(point_estimate, lower_bound, upper_bound, p_value, symbol_indicator){
  if(is.na(p_value)){
    return(NA)
  }else{
    if(p_value < 0.05){
      temp_text <- paste("underline( (", lower_bound)
      if(substr(lower_bound, nchar(lower_bound), nchar(lower_bound)) == "0"){
        if(substr(lower_bound, nchar(lower_bound)-1, nchar(lower_bound)) == "00"){
          temp_text <- paste(temp_text, " *. *0 *0")
        }else{
          temp_text <- paste(temp_text, " *0")
        }
      }
      temp_text <- paste(temp_text, " *symbol(\"\U002C\")*~ ", upper_bound)
      if(substr(upper_bound, nchar(upper_bound), nchar(upper_bound)) == "0"){
        if(substr(upper_bound, nchar(upper_bound)-1, nchar(upper_bound)) == "00"){
          temp_text <- paste(temp_text, " *. *0 *0")
        }else{
          temp_text <- paste(temp_text, " *0")
        }
      }
      temp_text <- paste(temp_text, " ) )")
    }else{
      temp_text <- paste("(", lower_bound)
      if(substr(lower_bound, nchar(lower_bound), nchar(lower_bound)) == "0"){
        if(substr(lower_bound, nchar(lower_bound)-1, nchar(lower_bound)) == "00"){
          temp_text <- paste(temp_text, " *. *0 *0")
        }else{
          temp_text <- paste(temp_text, " *0")
        }
      }
      temp_text <- paste(temp_text, " *symbol(\"\U002C\")*~ ", upper_bound)
      if(substr(upper_bound, nchar(upper_bound), nchar(upper_bound)) == "0"){
        if(substr(upper_bound, nchar(upper_bound)-1, nchar(upper_bound)) == "00"){
          temp_text <- paste(temp_text, " *. *0 *0")
        }else{
          temp_text <- paste(temp_text, " *0")
        }
      }
      temp_text <- paste(temp_text, " )")
    }
    return(temp_text)
  }
}
