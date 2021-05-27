#' Visualization of the network meta-analysis using plate plot
#' @import ggplot2
#' @param nma_result the network meta-analysis results, which needs to be visualized
#' @param null_value_zero a vector of indicators for upper diagonal matrix and lower diagonal matrix
#'   null_value_zero = 1 (TRUE) means results are log odds ratio, risk difference, mean difference
#'   null_value_zero = 0 (FALSE) means results are odds ratio, risk ratio
#'   Allowed values are c(upper diagonal matrix, lower diagonal matrix): c(0, 0), c(1, 0), c(0, 1), c(1, 1)
#' @param lower_better a vector of indicators for upper diagonal matrix and lower diagonal matrix
#'   e.g. if the result is risk ratio (RR) between treatment i and treatment j, then
#'   lower_better = 1 (TRUE) means if RR < 1 result favors treatment i
#'   lower_better = 0 (FALSE) means if RR < 1 results favors treatment j
#'   Allowed values are c(upper diagonal matrix, lower diagonal matrix): c(0, 0), c(1, 0), c(0, 1), c(1, 1)
#' @param design_method a vector of indicators for upper diagonal matrix and lower diagonal matrix
#'   design_method = "cicle" means plate plot is drawn
#'   design_method = "text" means text is displayed
#'   Allowed values are c(upper diagonal matrix, lower diagonal matrix):
#'   c("circle", "text"), c("circle", "circle"), c("text", "circle"), c("text", "text")
#' @param plate_circle_minsize a vector of circle minimum size for upper diagonal matrix and lower diagonal matrix
#'   Allowed values are positive real numbers, c(upper diagonal matrix, lower diagonal matrix): c(c1, c2)
#' @param plate_circle_maxsize a vector of circle maximum size for upper diagonal matrix and lower diagonal matrix
#'   Allowed values are positive real numbers, c(upper diagonal matrix, lower diagonal matrix): c(c1, c2)
#' @param plate_circle_samesize a boolean data to indicate
#'   whether the plate plots in lower diagonal matrix and upper diagonal matrix is consistent or not
#'   Allowed values are TRUE (1) or FALSE (0)
#' @param transform_rc_ullr_boolean a boolean data to indicate
#'   whether you want to transform the dataset from row-column type to upperleft-lowerright type
#'   Allowed values are TRUE (1) or FALSE (0)
#' @param text_size text size in the plot
#'   Allowed values are positive real numbers
#' @param max_substring display first several (max_substring) characters of treatment names
#'   Allowed values are positive integers
#' @param title the title of plot
#'   Allowed values are strings
#' @param bold make the text bold or not
#'   Allowed values are TRUE or FALSE
#' @param upper_diagonal_name the name of upper diagonal plot
#'   Allowed values are strings
#' @param lower_diagonal_name the name of lower diagonal plot
#'   Allowed values are strings
#' @param diagonal_color the colors (low and high) to indicate rankings of treatments
#'   Allowed values are color names, e.g. c("#F0E2E6", "#E51D8E") stands for the color ranges from
#'   "#F0E2E6" (lowest) to "#E51D8E" (highest)
#' @param offdiagonal_color the background colors to indicate difference between upper diagonal and lower diagonal
#'   Allowed values are color names, e.g. c("khaki", "cornsilk") means the background colors for
#'   upper diagonal cells and lower diagonal cells are "khaki" and "cornsilk" respectively
#' @param text_and_circle_color the colors (low and high) to indicate plates
#'   Allowed values are color names, e.g. c("red", "grey10", "blue", "grey70", "white"):
#'   c("red", "grey10", "blue"), a vector of 3 colors for low, mid and high p-values;
#'   "grey70", color for circle represents point estimate
#'   "white", color for circle represents lower bound of interval estimate
#' @return
#' \itemize{
#'  \item nmaplateplot(): Returns a ggplot2
#'  }

# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

plateplot <- function(nma_result,
                         null_value_zero = c(FALSE, FALSE),
                         lower_better = c(FALSE, TRUE),
                         design_method = c("circle", "circle"),
                         plate_circle_minsize = c(2.0, 2.0),
                         plate_circle_maxsize = c(13.0, 13.0),
                         plate_circle_samesize = FALSE,
                         transform_rc_ullr_boolean = TRUE,
                         text_size = 3.0,
                         bold = FALSE,
                         max_substring = 4,
                         title = NA,
                         upper_diagonal_name = NA,
                         lower_diagonal_name = NA,
                         diagonal_color = c("#F0E2E6", "#E51D8E"),
                         offdiagonal_color = c("khaki", "cornsilk"),
                         text_and_circle_color = c("red", "grey10", "blue", "grey70", "white")){

  ##### check the data
  if(missing(nma_result)){
    stop("please provide the data set for nma_result.")
  }else{
    Results <- nma_result
  }
  if(!inherits(Results, "list")){
    stop("nma_result needs to be a list.")
  }

  if(is.null(Results$Point_estimates) |
     is.null(Results$Interval_estimates_LB) |
     is.null(Results$Interval_estimates_UB) |
     is.null(Results$Pvalues) |
     is.null(Results$Treatment_specific_values)){
    stop("nma_result needs to contain five data frames (Point_estimates,
         Interval_estimates_LB, Interval_estimates_UB,
         Pvalues, Treatment_specific_values).")
  }

  if(!inherits(Results$Point_estimates, "data.frame")){
    stop("Point_estimates needs to be a data frame.")
  }
  if(!inherits(Results$Interval_estimates_LB, "data.frame")){
    stop("Interval_estimates_LB needs to be a data frame.")
  }
  if(!inherits(Results$Interval_estimates_UB, "data.frame")){
    stop("Interval_estimates_UB needs to be a data frame.")
  }
  if(!inherits(Results$Pvalues, "data.frame")){
    stop("Pvalues needs to be a data frame.")
  }
  if(!inherits(Results$Treatment_specific_values, "data.frame")){
    stop("Treatment_specific_values needs to be a data frame.")
  }

  if(is.null(Results$Treatment_specific_values$Trt_ID) |
     is.null(Results$Treatment_specific_values$Trt_abbrv)){
    stop("Treatment_specific_values needs to contain at least two columns (Trt_ID and Trt_abbrv).")
  }

  ##### generate the parameters
  ntrt <- nrow(Results$Treatment_specific_values)
  null_value_zero_uDM <- null_value_zero[1]
  null_value_zero_lDM <- null_value_zero[2]
  lower_better_uDM <- lower_better[1]
  lower_better_lDM <- lower_better[2]
  plate_circle_minsize_uDM <- plate_circle_minsize[1]
  plate_circle_minsize_lDM <- plate_circle_minsize[2]
  plate_circle_maxsize_uDM <- plate_circle_maxsize[1]
  plate_circle_maxsize_lDM <- plate_circle_maxsize[2]
  design_method_uDM <- design_method[1]
  design_method_lDM <- design_method[2]
  if(transform_rc_ullr_boolean){
    colorbar_name = "ullr"
  }else{
    colorbar_name = "rc"
  }
  ##### Tricks to make some vars to have visible binding
  x <- y <- text <- values <- id <- xend <- yend <- label <- color <- size <- NULL
  ##### clean the dataset
  ## automatically create the symbols
  if(is.null(Results$Symbol_indicators)){
    Results$Symbol_indicators <- as.data.frame(matrix(0, nrow = ntrt, ncol = ntrt))
    for(ii in 1:ntrt){
      for(jj in 1:ntrt){
        if(is.na(Results$Pvalues[ii, jj])){
          Results$Symbol_indicators[ii, jj] <- NA
        }
        else if(Results$Pvalues[ii, jj] > 0.05){
          Results$Symbol_indicators[ii, jj] <- NA
        }else if(Results$Pvalues[ii, jj] <= 0.05 & Results$Pvalues[ii, jj] > 0.01){
          Results$Symbol_indicators[ii, jj] <- "a"
        }else if(Results$Pvalues[ii, jj] <= 0.01 & Results$Pvalues[ii, jj] > 0.001){
          Results$Symbol_indicators[ii, jj] <- "b"
        }else{
          Results$Symbol_indicators[ii, jj] <- "c"
        }
      }
    }
  }

  ## automatically create the order
  if(is.null(Results$Treatment_specific_values$Order)){
    if(!is.null(Results$Treatment_specific_values$Value_Upper)){
      Results$Treatment_specific_values$Order <- rank(-Results$Treatment_specific_values$Value_Upper, ties.method = "first")
    }else if(!is.null(Results$Treatment_specific_values$Value_Lower)){
      Results$Treatment_specific_values$Order <- rank(-Results$Treatment_specific_values$Value_Lower, ties.method = "first")
    }else{
      Results$Treatment_specific_values$Order <- c(1:ntrt)
    }
  }

  ## re-order the results matrix
  Order_temp <- Results$Treatment_specific_values[,c("Trt_ID","Order")]
  Order_temp <- Order_temp[order(Order_temp$Order),"Trt_ID"]
  Results$Point_estimates <- .reorder_matrix(Results$Point_estimates, Order_temp, null_value_zero_uDM, null_value_zero_lDM)
  Results$Interval_estimates_LB <- .reorder_matrix(Results$Interval_estimates_LB, Order_temp, null_value_zero_uDM, null_value_zero_lDM)
  Results$Interval_estimates_UB <- .reorder_matrix(Results$Interval_estimates_UB, Order_temp, null_value_zero_uDM, null_value_zero_lDM)
  Results$Pvalues <- .reorder_matrix(Results$Pvalues, Order_temp, 2, 2)
  if(!is.null(Results$Symbol_indicators)){
    Results$Symbol_indicators <- .reorder_matrix(Results$Symbol_indicators, Order_temp, 2, 2)
  }

  ## Transform the dataset from row-column type to upperleft-lowerright type
  if(transform_rc_ullr_boolean == 1){
    Results <- .from_rc_to_ullr(Results = Results, null_value_zero = null_value_zero_lDM)
  }

  ##### create data frames for border lines
  ### vertical and horizontal border lines
  border_lines <- matrix(0, nrow = 2*(ntrt+1), ncol = 4)
  colnames(border_lines) <- c("x", "y", "xend", "yend")
  border_lines <- as.data.frame(border_lines)
  for(i in 0:ntrt){
    # horizontal lines
    i_temp_h <- i * 2 + 1
    border_lines$x[i_temp_h] <- 0
    border_lines$y[i_temp_h] <- i
    border_lines$xend[i_temp_h] <- ntrt
    border_lines$yend[i_temp_h] <- i
    # vertical lines
    i_temp_v <- i * 2 + 2
    border_lines$x[i_temp_v] <- i
    border_lines$y[i_temp_v] <- 0
    border_lines$xend[i_temp_v] <- i
    border_lines$yend[i_temp_v] <- ntrt
  }
  ### diagonal border lines
  border_lines_diagonal <- as.data.frame(matrix(c(0, ntrt, ntrt, 0), nrow = 1, ncol = 4))
  colnames(border_lines_diagonal) <- c("x", "y", "xend", "yend")


  ##### create data frames for off-diagonal matrix's background and name
  if(!is.na(upper_diagonal_name) | !is.na(lower_diagonal_name)){
    upperdiagonal_background <- data.frame(
      id = rep(1, each = 2 * ntrt),
      x = c(rep(c(1:ntrt), each = 2)),
      y = c(ntrt, rep(c((ntrt-1):1), each = 2), ntrt)
    )
    lowerdiagonal_background <- data.frame(
      id = rep(1, each = 2 * ntrt),
      x = c(rep(c(0:(ntrt-1)), each = 2)),
      y = c(0, rep(c((ntrt-1):1), each = 2), 0)
    )
  }

  ##### create data frames for diagonal triangles (upper and lower), and diagonal texts
  diagonal_type <- 0
  if(is.null(Results$Treatment_specific_values$Value_Upper) &
     is.null(Results$Treatment_specific_values$Value_Lower)){
    # Diagonal treatment abbreviations
    diagonal_text_abbr_values <- Results$Treatment_specific_values[, c("Trt_abbrv", "Order")]
    colnames(diagonal_text_abbr_values) <- c("text", "id")
    diagonal_text_abbr_values$text <- substr(diagonal_text_abbr_values$text, 1, max_substring)
    diagonal_text_abbr_position <- data.frame(
      id = c(1:ntrt),
      x = seq(from = 0.5, to = ntrt-0.5, by = 1),
      y = seq(from = ntrt-0.5, to = 0.5, by = -1)
    )
    diagonal_text_abbr <- merge(diagonal_text_abbr_values, diagonal_text_abbr_position, by = c("id"))
    diagonal_type <- 1
  }else if(is.null(Results$Treatment_specific_values$Value_Upper) &
           !is.null(Results$Treatment_specific_values$Value_Lower)){
    # diagonal square
    diagonal_value_square <- Results$Treatment_specific_values[, c("Value_Lower", "Order")]
    colnames(diagonal_value_square) <- c("values", "id")
    diagonal_position_square <- data.frame(
      id = rep(c(1:ntrt), each = 4),
      x = rep(c(0:ntrt), each = 4)[3:(2+ntrt*4)],
      y_temp = rep(c(ntrt:0), each = 4)[3:(2+ntrt*4)],
      y = 0
    )
    for(ii in 1:ntrt){
      diagonal_position_square$y[1+(ii-1)*4] <- diagonal_position_square$y_temp[3+(ii-1)*4]
      diagonal_position_square$y[2+(ii-1)*4] <- diagonal_position_square$y_temp[1+(ii-1)*4]
      diagonal_position_square$y[3+(ii-1)*4] <- diagonal_position_square$y_temp[2+(ii-1)*4]
      diagonal_position_square$y[4+(ii-1)*4] <- diagonal_position_square$y_temp[4+(ii-1)*4]
    }
    diagonal_position_square$id_temp <- 1:nrow(diagonal_position_square)
    diagonal_square <- merge(diagonal_value_square, diagonal_position_square, by = c("id"))
    diagonal_square <- diagonal_square[order(diagonal_square$id_temp), ]

    # Diagonal treatment abbreviations
    diagonal_text_abbr_values <- Results$Treatment_specific_values[, c("Trt_abbrv", "Value_Lower", "Order")]
    colnames(diagonal_text_abbr_values) <- c("text", "value", "id")
    diagonal_text_abbr_values$text <- substr(diagonal_text_abbr_values$text, 1, max_substring)
    diagonal_text_abbr_position <- data.frame(
      id = c(1:ntrt),
      x = seq(from = 0.5, to = ntrt-0.5, by = 1),
      y = seq(from = ntrt-0.5, to = 0.5, by = -1)
    )
    diagonal_text_abbr <- merge(diagonal_text_abbr_values, diagonal_text_abbr_position, by = c("id"))
    diagonal_text_abbr$text <- paste0(diagonal_text_abbr$text, "\n",
                                      round(diagonal_text_abbr$value * 100))
    diagonal_type <- 2

  }else if(!is.null(Results$Treatment_specific_values$Value_Upper) &
           is.null(Results$Treatment_specific_values$Value_Lower)){
    # diagonal square
    diagonal_value_square <- Results$Treatment_specific_values[, c("Value_Upper", "Order")]
    colnames(diagonal_value_square) <- c("values", "id")
    diagonal_position_square <- data.frame(
      id = rep(c(1:ntrt), each = 4),
      x = rep(c(0:ntrt), each = 4)[3:(2+ntrt*4)],
      y_temp = rep(c(ntrt:0), each = 4)[3:(2+ntrt*4)],
      y = 0
    )
    for(ii in 1:ntrt){
      diagonal_position_square$y[1+(ii-1)*4] <- diagonal_position_square$y_temp[3+(ii-1)*4]
      diagonal_position_square$y[2+(ii-1)*4] <- diagonal_position_square$y_temp[1+(ii-1)*4]
      diagonal_position_square$y[3+(ii-1)*4] <- diagonal_position_square$y_temp[2+(ii-1)*4]
      diagonal_position_square$y[4+(ii-1)*4] <- diagonal_position_square$y_temp[4+(ii-1)*4]
    }
    diagonal_position_square$id_temp <- 1:nrow(diagonal_position_square)
    diagonal_square <- merge(diagonal_value_square, diagonal_position_square, by = c("id"))
    diagonal_square <- diagonal_square[order(diagonal_square$id_temp), ]

    # Diagonal treatment abbreviations
    diagonal_text_abbr_values <- Results$Treatment_specific_values[, c("Trt_abbrv", "Value_Upper", "Order")]
    colnames(diagonal_text_abbr_values) <- c("text", "value", "id")
    diagonal_text_abbr_values$text <- substr(diagonal_text_abbr_values$text, 1, max_substring)
    diagonal_text_abbr_position <- data.frame(
      id = c(1:ntrt),
      x = seq(from = 0.5, to = ntrt-0.5, by = 1),
      y = seq(from = ntrt-0.5, to = 0.5, by = -1)
    )
    diagonal_text_abbr <- merge(diagonal_text_abbr_values, diagonal_text_abbr_position, by = c("id"))
    diagonal_text_abbr$text <- paste0(diagonal_text_abbr$text, "\n",
                                      round(diagonal_text_abbr$value * 100))
    diagonal_type <- 2


  }else if(!is.null(Results$Treatment_specific_values$Value_Upper) &
           !is.null(Results$Treatment_specific_values$Value_Lower) &
           !isTRUE(all.equal(Results$Treatment_specific_values$Value_Upper, Results$Treatment_specific_values$Value_Lower))){
    # upper triangle
    diagonal_value_upper <- Results$Treatment_specific_values[, c("Value_Upper", "Order")]
    colnames(diagonal_value_upper) <- c("values", "id")
    diagonal_positions_upper <- data.frame(
      id = rep(c(1:ntrt), each = 3),
      x = rep(c(0:ntrt), each = 3)[3:(2+ntrt*3)],
      y = rep(c(ntrt:0), each = 3)[2:(1+ntrt*3)]
    )
    diagonal_poly_upper <- merge(diagonal_value_upper, diagonal_positions_upper, by = c("id"))
    # lower triangle
    diagonal_value_lower <- Results$Treatment_specific_values[, c("Value_Lower", "Order")]
    colnames(diagonal_value_lower) <- c("values", "id")
    diagonal_positions_lower <- data.frame(
      id = rep(c(1:ntrt), each = 3),
      x = rep(c(0:ntrt), each = 3)[2:(1+ntrt*3)],
      y = rep(c(ntrt:0), each = 3)[3:(2+ntrt*3)]
    )
    diagonal_poly_lower <- merge(diagonal_value_lower, diagonal_positions_lower, by = c("id"))

    # Diagonal treatment abbreviations
    diagonal_text_abbr_values <- Results$Treatment_specific_values[, c("Trt_abbrv", "Order")]
    colnames(diagonal_text_abbr_values) <- c("text", "id")
    diagonal_text_abbr_values$text <- substr(diagonal_text_abbr_values$text, 1, max_substring)
    diagonal_text_abbr_position <- data.frame(
      id = c(1:ntrt),
      x = seq(from = 0.5, to = ntrt-0.5, by = 1),
      y = seq(from = ntrt-0.5, to = 0.5, by = -1)
    )
    diagonal_text_abbr <- merge(diagonal_text_abbr_values, diagonal_text_abbr_position, by = c("id"))

    # Diagonal values (Upper poly)
    diagonal_text_upper_values <- Results$Treatment_specific_values[, c("Value_Upper", "Order")]
    colnames(diagonal_text_upper_values) <- c("values", "id")
    diagonal_text_upper_values$values <- round(diagonal_text_upper_values$values * 100)
    diagonal_text_upper_position <- data.frame(
      id = c(1:ntrt),
      x = seq(from = 0.8, to = ntrt-0.2, by = 1),
      y = seq(from = ntrt-0.2, to = 0.8, by = -1)
    )
    diagonal_text_upper <- merge(diagonal_text_upper_values, diagonal_text_upper_position, by = c("id"))
    # Diagonal values (Lower poly)
    diagonal_text_lower_values <- Results$Treatment_specific_values[, c("Value_Lower", "Order")]
    colnames(diagonal_text_lower_values) <- c("values", "id")
    diagonal_text_lower_values$values <- round(diagonal_text_lower_values$values * 100)
    diagonal_text_lower_position <- data.frame(
      id = c(1:ntrt),
      x = seq(from = 0.2, to = ntrt-0.8, by = 1),
      y = seq(from = ntrt-0.8, to = 0.2, by = -1)
    )
    diagonal_text_lower <- merge(diagonal_text_lower_values, diagonal_text_lower_position, by = c("id"))
    diagonal_type <- 3

  }else{
    # diagonal square
    diagonal_value_square <- Results$Treatment_specific_values[, c("Value_Upper", "Order")]
    colnames(diagonal_value_square) <- c("values", "id")
    diagonal_position_square <- data.frame(
      id = rep(c(1:ntrt), each = 4),
      x = rep(c(0:ntrt), each = 4)[3:(2+ntrt*4)],
      y_temp = rep(c(ntrt:0), each = 4)[3:(2+ntrt*4)],
      y = 0
    )
    for(ii in 1:ntrt){
      diagonal_position_square$y[1+(ii-1)*4] <- diagonal_position_square$y_temp[3+(ii-1)*4]
      diagonal_position_square$y[2+(ii-1)*4] <- diagonal_position_square$y_temp[1+(ii-1)*4]
      diagonal_position_square$y[3+(ii-1)*4] <- diagonal_position_square$y_temp[2+(ii-1)*4]
      diagonal_position_square$y[4+(ii-1)*4] <- diagonal_position_square$y_temp[4+(ii-1)*4]
    }
    diagonal_position_square$id_temp <- 1:nrow(diagonal_position_square)
    diagonal_square <- merge(diagonal_value_square, diagonal_position_square, by = c("id"))
    diagonal_square <- diagonal_square[order(diagonal_square$id_temp), ]

    # Diagonal treatment abbreviations
    diagonal_text_abbr_values <- Results$Treatment_specific_values[, c("Trt_abbrv", "Value_Upper", "Order")]
    colnames(diagonal_text_abbr_values) <- c("text", "value", "id")
    diagonal_text_abbr_values$text <- substr(diagonal_text_abbr_values$text, 1, max_substring)
    diagonal_text_abbr_position <- data.frame(
      id = c(1:ntrt),
      x = seq(from = 0.5, to = ntrt-0.5, by = 1),
      y = seq(from = ntrt-0.5, to = 0.5, by = -1)
    )
    diagonal_text_abbr <- merge(diagonal_text_abbr_values, diagonal_text_abbr_position, by = c("id"))
    diagonal_text_abbr$text <- paste0(diagonal_text_abbr$text, "\n",
                                      round(diagonal_text_abbr$value * 100))
    diagonal_type <- 2
  }

  ##### create data frames for the plateplot
  ### Upper diagonal matrix
  # 1,2, and 3 represents first (largest), second, and third (smallest) circles to be plotted
  upper_dm_plate1 <- as.data.frame(matrix(0, nrow = (ntrt-1)*ntrt/2, ncol = 4))
  colnames(upper_dm_plate1) <- c("x","y","size","color")
  upper_dm_plate2 <- as.data.frame(matrix(0, nrow = (ntrt-1)*ntrt/2, ncol = 4))
  colnames(upper_dm_plate2) <- c("x", "y", "size", "color")
  upper_dm_plate3 <- as.data.frame(matrix(0, nrow = (ntrt-1)*ntrt/2, ncol = 4))
  colnames(upper_dm_plate3) <- c("x", "y", "size", "color")
  temp_i <- 0
  for(i in 1:(ntrt-1)){
    for(j in (i+1):ntrt){
      temp_i <- temp_i+1
      temp_LB <- Results$Interval_estimates_LB[i, j]
      temp_UB <- Results$Interval_estimates_UB[i, j]
      temp_est <- Results$Point_estimates[i, j]
      temp_p <- Results$Pvalues[i, j]

      temp_color <- .pvalue_color_conversion(temp_p, temp_est, null_value_zero_uDM, lower_better_uDM)
      temp_size <- numeric(3)
      temp_size[1] <- .pe_size_conversion(temp_LB, temp_est, null_value_zero_uDM)
      temp_size[2] <- .pe_size_conversion(temp_est, temp_est, null_value_zero_uDM)
      temp_size[3] <- .pe_size_conversion(temp_UB, temp_est, null_value_zero_uDM)
      temp_size <- temp_size[order(-temp_size)]

      temp_x <- j - 0.5
      temp_y <- (ntrt - i) + 0.5

      upper_dm_plate1$x[temp_i] <- temp_x
      upper_dm_plate2$x[temp_i] <- temp_x
      upper_dm_plate3$x[temp_i] <- temp_x
      upper_dm_plate1$y[temp_i] <- temp_y
      upper_dm_plate2$y[temp_i] <- temp_y
      upper_dm_plate3$y[temp_i] <- temp_y
      upper_dm_plate1$size[temp_i] <- temp_size[1]
      upper_dm_plate2$size[temp_i] <- temp_size[2]
      upper_dm_plate3$size[temp_i] <- temp_size[3]
      upper_dm_plate1$color[temp_i] <- temp_color
      upper_dm_plate2$color[temp_i] <- temp_color
      upper_dm_plate3$color[temp_i] <- temp_color
    }
  }
  ### Lower diagonal matrix
  lower_dm_plate1 <- as.data.frame(matrix(0, nrow = (ntrt-1)*ntrt/2, ncol = 4))
  colnames(lower_dm_plate1) <- c("x","y","size","color")
  lower_dm_plate2 <- as.data.frame(matrix(0, nrow = (ntrt-1)*ntrt/2, ncol = 4))
  colnames(lower_dm_plate2) <- c("x", "y", "size", "color")
  lower_dm_plate3 <- as.data.frame(matrix(0, nrow = (ntrt-1)*ntrt/2, ncol = 4))
  colnames(lower_dm_plate3) <- c("x", "y", "size", "color")
  temp_i <- 0
  for(i in 2:ntrt){
    for(j in 1:(i-1)){
      temp_i <- temp_i+1
      temp_LB <- Results$Interval_estimates_LB[i, j]
      temp_UB <- Results$Interval_estimates_UB[i, j]
      temp_est <- Results$Point_estimates[i, j]
      temp_p <- Results$Pvalues[i, j]

      temp_color <- .pvalue_color_conversion(temp_p, temp_est, null_value_zero_lDM, lower_better_lDM)
      temp_size <- numeric(3)
      temp_size[1] <- .pe_size_conversion(temp_LB, temp_est, null_value_zero_lDM)
      temp_size[2] <- .pe_size_conversion(temp_est, temp_est, null_value_zero_lDM)
      temp_size[3] <- .pe_size_conversion(temp_UB, temp_est, null_value_zero_lDM)
      temp_size <- temp_size[order(-temp_size)]

      temp_x <- j - 0.5
      temp_y <- (ntrt - i) + 0.5

      lower_dm_plate1$x[temp_i] <- temp_x
      lower_dm_plate2$x[temp_i] <- temp_x
      lower_dm_plate3$x[temp_i] <- temp_x
      lower_dm_plate1$y[temp_i] <- temp_y
      lower_dm_plate2$y[temp_i] <- temp_y
      lower_dm_plate3$y[temp_i] <- temp_y
      lower_dm_plate1$size[temp_i] <- temp_size[1]
      lower_dm_plate2$size[temp_i] <- temp_size[2]
      lower_dm_plate3$size[temp_i] <- temp_size[3]
      lower_dm_plate1$color[temp_i] <- temp_color
      lower_dm_plate2$color[temp_i] <- temp_color
      lower_dm_plate3$color[temp_i] <- temp_color
    }
  }
  ### adjust the plate size
  if(plate_circle_samesize & design_method_uDM == "circle" & design_method_lDM == "circle"){
    temp_min_size <- c(lower_dm_plate1$size, lower_dm_plate2$size, lower_dm_plate3$size,
                       upper_dm_plate1$size, upper_dm_plate2$size, upper_dm_plate3$size)
    temp_min_size <- min(temp_min_size[temp_min_size > 0])
    temp_max_size <- max(c(lower_dm_plate1$size, lower_dm_plate2$size, lower_dm_plate3$size,
                           upper_dm_plate1$size, upper_dm_plate2$size, upper_dm_plate3$size))
    temp_k <- (plate_circle_maxsize_uDM - plate_circle_minsize_uDM) / (temp_max_size - temp_min_size)
    temp_b <- (plate_circle_minsize_uDM * temp_max_size -
               plate_circle_maxsize_uDM * temp_min_size) / (temp_max_size - temp_min_size)

    lower_dm_plate1$size <- ifelse(lower_dm_plate1$size == 0, NA, lower_dm_plate1$size)
    lower_dm_plate2$size <- ifelse(lower_dm_plate2$size == 0, NA, lower_dm_plate2$size)
    lower_dm_plate3$size <- ifelse(lower_dm_plate3$size == 0, NA, lower_dm_plate3$size)
    lower_dm_plate1$size <- lower_dm_plate1$size * temp_k + temp_b
    lower_dm_plate2$size <- lower_dm_plate2$size * temp_k + temp_b
    lower_dm_plate3$size <- lower_dm_plate3$size * temp_k + temp_b

    upper_dm_plate1$size <- ifelse(upper_dm_plate1$size == 0, NA, upper_dm_plate1$size)
    upper_dm_plate2$size <- ifelse(upper_dm_plate2$size == 0, NA, upper_dm_plate2$size)
    upper_dm_plate3$size <- ifelse(upper_dm_plate3$size == 0, NA, upper_dm_plate3$size)
    upper_dm_plate1$size <- upper_dm_plate1$size * temp_k + temp_b
    upper_dm_plate2$size <- upper_dm_plate2$size * temp_k + temp_b
    upper_dm_plate3$size <- upper_dm_plate3$size * temp_k + temp_b
  }else{
    if(design_method_lDM == "circle"){
      temp_min_size <- c(lower_dm_plate1$size, lower_dm_plate2$size, lower_dm_plate3$size)
      temp_min_size <- min(temp_min_size[temp_min_size > 0])
      temp_max_size <- max(c(lower_dm_plate1$size, lower_dm_plate2$size, lower_dm_plate3$size))
      temp_k <- (plate_circle_maxsize_lDM - plate_circle_minsize_lDM) / (temp_max_size - temp_min_size)
      temp_b <- (plate_circle_minsize_lDM * temp_max_size -
                   plate_circle_maxsize_lDM * temp_min_size) / (temp_max_size - temp_min_size)

      lower_dm_plate1$size <- ifelse(lower_dm_plate1$size == 0, NA, lower_dm_plate1$size)
      lower_dm_plate2$size <- ifelse(lower_dm_plate2$size == 0, NA, lower_dm_plate2$size)
      lower_dm_plate3$size <- ifelse(lower_dm_plate3$size == 0, NA, lower_dm_plate3$size)
      lower_dm_plate1$size <- lower_dm_plate1$size * temp_k + temp_b
      lower_dm_plate2$size <- lower_dm_plate2$size * temp_k + temp_b
      lower_dm_plate3$size <- lower_dm_plate3$size * temp_k + temp_b
    }
    if(design_method_uDM == "circle"){
      temp_min_size <- c(upper_dm_plate1$size, upper_dm_plate2$size, upper_dm_plate3$size)
      temp_min_size <- min(temp_min_size[temp_min_size > 0])
      temp_max_size <- max(c(upper_dm_plate1$size, upper_dm_plate2$size, upper_dm_plate3$size))
      temp_k <- (plate_circle_maxsize_uDM - plate_circle_minsize_uDM) / (temp_max_size - temp_min_size)
      temp_b <- (plate_circle_minsize_uDM * temp_max_size -
                   plate_circle_maxsize_uDM * temp_min_size) / (temp_max_size - temp_min_size)

      upper_dm_plate1$size <- ifelse(upper_dm_plate1$size == 0, NA, upper_dm_plate1$size)
      upper_dm_plate2$size <- ifelse(upper_dm_plate2$size == 0, NA, upper_dm_plate2$size)
      upper_dm_plate3$size <- ifelse(upper_dm_plate3$size == 0, NA, upper_dm_plate3$size)
      upper_dm_plate1$size <- upper_dm_plate1$size * temp_k + temp_b
      upper_dm_plate2$size <- upper_dm_plate2$size * temp_k + temp_b
      upper_dm_plate3$size <- upper_dm_plate3$size * temp_k + temp_b
    }
  }

  ##### Write the Point estimate with 95% credible Interval
  ### Lower Diagonal Matrix
  lower_DM_text <- as.data.frame(matrix(0, nrow = (ntrt-1)*ntrt, ncol = 4))
  colnames(lower_DM_text) <- c("x","y","text","color")
  temp_i <- 0
  for(i in 2:ntrt){
    for(j in 1:(i-1)){
      temp_i <- temp_i+1
      temp_LB <- Results$Interval_estimates_LB[i, j]
      temp_UB <- Results$Interval_estimates_UB[i, j]
      temp_est <- Results$Point_estimates[i, j]
      temp_p <- Results$Pvalues[i, j]
      temp_symbol <- Results$Symbol_indicators[i, j]

      temp_color <- .pvalue_color_conversion(temp_p, temp_est, null_value_zero_lDM, lower_better_lDM)


      temp_est <- format(round(temp_est, 2), nsmall = 2)
      if(is.na(temp_LB) | is.na(temp_UB)){
        temp_LB_n <- NA
        temp_UB_n <- NA
      }else if(temp_LB < temp_UB){
	      temp_LB_n <- format(round(temp_LB, 2), nsmall = 2)
		    temp_UB_n <- format(round(temp_UB, 2), nsmall = 2)
	    }else{
	      temp_LB_n <- format(round(temp_UB, 2), nsmall = 2)
		    temp_UB_n <- format(round(temp_LB, 2), nsmall = 2)
	    }

      lower_DM_text$text[temp_i] <- .pe_text_conversion_u(temp_est, temp_LB_n, temp_UB_n, temp_p, temp_symbol)
      lower_DM_text$x[temp_i] <- j - 0.5
      lower_DM_text$y[temp_i] <- (ntrt - i) + 0.75
      lower_DM_text$color[temp_i] <- temp_color

      temp_i <- temp_i+1
      lower_DM_text$text[temp_i] <- .pe_text_conversion_l(temp_est, temp_LB_n, temp_UB_n, temp_p, temp_symbol)
      lower_DM_text$x[temp_i] <- j - 0.5
      lower_DM_text$y[temp_i] <- (ntrt - i) + 0.25
      lower_DM_text$color[temp_i] <- temp_color
    }
  }
  ### Upper Diagonal Matrix
  upper_DM_text <- as.data.frame(matrix(0, nrow = (ntrt-1)*ntrt, ncol = 4))
  colnames(upper_DM_text) <- c("x","y","text","color")
  temp_i <- 0
  for(i in 1:(ntrt-1)){
    for(j in (i+1):ntrt){
      temp_i <- temp_i+1
      temp_LB <- Results$Interval_estimates_LB[i, j]
      temp_UB <- Results$Interval_estimates_UB[i, j]
      temp_est <- Results$Point_estimates[i, j]
      temp_p <- Results$Pvalues[i, j]
      temp_symbol <- Results$Symbol_indicators[i, j]

      temp_color <- .pvalue_color_conversion(temp_p, temp_est, null_value_zero_uDM, lower_better_uDM)


      temp_est <- format(round(temp_est, 2), nsmall = 2)
      if(is.na(temp_LB) | is.na(temp_UB)){
        temp_LB_n <- NA
        temp_UB_n <- NA
      }else if(temp_LB < temp_UB){
	      temp_LB_n <- format(round(temp_LB, 2), nsmall = 2)
		    temp_UB_n <- format(round(temp_UB, 2), nsmall = 2)
	    }else{
	      temp_LB_n <- format(round(temp_UB, 2), nsmall = 2)
		    temp_UB_n <- format(round(temp_LB, 2), nsmall = 2)
	    }

      upper_DM_text$text[temp_i] <- .pe_text_conversion_u(temp_est, temp_LB_n, temp_UB_n, temp_p, temp_symbol)
      upper_DM_text$x[temp_i] <- j - 0.5
      upper_DM_text$y[temp_i] <- (ntrt - i) + 0.75
      upper_DM_text$color[temp_i] <- temp_color

      temp_i <- temp_i+1
      upper_DM_text$text[temp_i] <- .pe_text_conversion_l(temp_est, temp_LB_n, temp_UB_n, temp_p, temp_symbol)
      upper_DM_text$x[temp_i] <- j - 0.5
      upper_DM_text$y[temp_i] <- (ntrt - i) + 0.25
      upper_DM_text$color[temp_i] <- temp_color
    }
  }


  ##### start plotting
  ### draw the border lines
  p <-
    ggplot2::ggplot()

  ### draw the diagonal
  if(diagonal_type == 1){
    p <- p+
      # diagonal_text_abbrv_size
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text),
                         size = text_size*8/7, fontface = "bold", data = diagonal_text_abbr)

  }else if(diagonal_type == 2){
    p <- p+
      # if else Value_lower Value_Upper exists
      ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, fill = values, group = id), data = diagonal_square)+
      # fill_LB="#F0E2E6"
      # fill_UB="#E51D8E"
      # diagonal_rank_name="SUCRA(%)"
      # diagonal_rank_breaks
      # diagonal_rank_limits
      # diagonal_rank_labels
      ggplot2::scale_fill_gradient(low = diagonal_color[1], high = diagonal_color[2], na.value = "transparent",
                                   breaks = c(0, 0.25, 0.5, 0.75, 1),
                                   labels = c(0, 25, 50, 75, 100),
                                   limits = c(0, 1), name="SUCRA(%)")+
      # diagonal_text_abbrv_size
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text),
                         size = text_size*8/7, fontface = "bold", data = diagonal_text_abbr)


  }else if(diagonal_type == 3){
    p <- p+
      # if else Value_lower Value_Upper exists
      ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, fill = values, group = id), data = diagonal_poly_upper)+
      ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, fill = values, group = id), data = diagonal_poly_lower)+
      # diagonal_rank_name="SUCRA(%)"
      ggplot2::scale_fill_gradient(low = diagonal_color[1], high = diagonal_color[2], na.value = "transparent",
                                   breaks = c(0, 0.25, 0.5, 0.75, 1),
                                   labels = c(0, 25, 50, 75, 100),
                                   limits = c(0, 1), name="SUCRA(%)")+
      # include diagonal lines or not
      ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            color = "white", size = 1.5, data = border_lines_diagonal)+
      # diagonal_text_abbrv_size
      # diagonal_text_numbers_size
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text), size = text_size*8/7, fontface = "bold", data = diagonal_text_abbr)+
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = values), size = text_size*6/7, fontface = "bold", data = diagonal_text_upper)+
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = values), size = text_size*6/7, fontface = "bold", data = diagonal_text_lower)
  }

  ### draw the offdiagonal background and names
  if(TRUE){
    diagonal_name_df <- data.frame(x = 0, y = ntrt + 0.25, label = "Treatment names")
    p <- p + ggplot2::geom_label(ggplot2::aes(x = x, y = y, label = label), colour = "black",
                                 fill = diagonal_color[2], fontface = "bold", size = text_size,
                                 hjust = 0, vjust = 0.5,
                                 data = diagonal_name_df)
  }
  if(!is.na(upper_diagonal_name)){
    p <- p + ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, group = id), fill = offdiagonal_color[1],
                                   data = upperdiagonal_background)
    upperdiagonal_name_df <- data.frame(x = ntrt, y = ntrt + 0.25, label = upper_diagonal_name)
    p <- p + ggplot2::geom_label(ggplot2::aes(x = x, y = y, label = label), colour = "black",
                                 fill = offdiagonal_color[1], fontface = "bold", size = text_size,
                                 hjust = 1.0, vjust = 0.5,
                                 data = upperdiagonal_name_df)
  }
  if(!is.na(lower_diagonal_name)){
    p <- p + ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, group = id), fill = offdiagonal_color[2],
                                   data = lowerdiagonal_background)
    lowerdiagonal_name_df <- data.frame(x = 0, y = -0.25, label = lower_diagonal_name)
    p <- p + ggplot2::geom_label(ggplot2::aes(x = x, y = y, label = label), colour = "black",
                                 fill = offdiagonal_color[2], fontface = "bold", size = text_size,
                                 hjust = 0, vjust = 0.5,
                                 data = lowerdiagonal_name_df)
  }
  ### draw the lower diagonal matrix
  if(design_method_lDM == "circle"){
    p <- p+
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, colour = color, size = size), data = lower_dm_plate1, na.rm = TRUE)+
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, size = size),
                          colour = text_and_circle_color[4],  data = lower_dm_plate2, na.rm = TRUE)+
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, size = size),
                          colour = text_and_circle_color[5], data = lower_dm_plate3, na.rm = TRUE)
  }else if(design_method_lDM == "text"){
    p <- p+
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text, colour = color),
                         size = text_size, data = lower_DM_text, parse = TRUE, na.rm = TRUE)
    if(bold == TRUE){
      p <- p+
        ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text, colour = color),
                                size = text_size + text_size/140, data = lower_DM_text, parse = TRUE, na.rm = TRUE)+
        ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text, colour = color),
                           size = text_size + text_size/70, data = lower_DM_text, parse = TRUE, na.rm = TRUE)
    }

  }

  ### draw the upper diagonal matrix
  if(design_method_uDM == "circle"){
    p <- p+
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, colour = color, size = size), data = upper_dm_plate1, na.rm = TRUE)+
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, size = size),
                          colour = text_and_circle_color[4],  data = upper_dm_plate2, na.rm = TRUE)+
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, size = size),
                          colour = text_and_circle_color[5], data = upper_dm_plate3, na.rm = TRUE)
  }else if(design_method_uDM == "text"){
    p <- p+
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text, colour = color),
                         size = text_size, data = upper_DM_text, parse = TRUE, na.rm = TRUE)
    if(bold == TRUE){
      p <- p+
        ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text, colour = color),
                           size = text_size + text_size/140, data = upper_DM_text, parse = TRUE, na.rm = TRUE)+
        ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text, colour = color),
                           size = text_size + text_size/70, data = upper_DM_text, parse = TRUE, na.rm = TRUE)
    }

  }

  ### draw the color bar
  if(colorbar_name == "rc"){
    p <- p+
      # diagonal_rank_name="P value"
      # log(1 - log(0.2, base = 1.5), base = 1.5)/log(1 - log(0.0001, base = 1.5), base = 1.5)
      ggplot2::scale_color_gradient2(low = text_and_circle_color[1], mid = text_and_circle_color[2],
                                     high = text_and_circle_color[3], na.value = "transparent",
                                     breaks = c(-1, -0.913546, -0.7941191, -0.6717503, -0.5063886, 0,
                                                0.5063886, 0.6717503, 0.7941191, 0.913546, 1),
                                     labels = c("Favors column", "0.001", "0.01", "0.05", "0.20", "Neutral",
                                                "0.20", "0.05", "0.01", "0.001", "Favors row"),
                                     limits = c(-1, 1), name="P value")+
      ggplot2::guides(color = ggplot2::guide_colorbar(barheight = grid::unit(260, "points")),
                      fill  = ggplot2::guide_colorbar(barheight = grid::unit(60, "points")))
  }else if(colorbar_name == "ullr"){
    p <- p+
      # diagonal_rank_name="P value"
      # log(1 - log(0.2, base = 1.5), base = 1.5)/log(1 - log(0.0001, base = 1.5), base = 1.5)
      ggplot2::scale_color_gradient2(low = text_and_circle_color[1], mid = text_and_circle_color[2],
                                     high = text_and_circle_color[3], na.value = "transparent",
                                     breaks = c(-1, -0.913546, -0.7941191, -0.6717503, -0.5063886, 0,
                                                0.5063886, 0.6717503, 0.7941191, 0.913546, 1),
                                     labels = c("Favors \nlower-right", "0.001", "0.01", "0.05", "0.20", "Neutral",
                                                "0.20", "0.05", "0.01", "0.001", "Favors \nupper-left"),
                                     limits = c(-1, 1), name="P value")+
      ggplot2::guides(color = ggplot2::guide_colorbar(barheight = grid::unit(260, "points")),
                      fill  = ggplot2::guide_colorbar(barheight = grid::unit(60, "points")))
  }

  if(!is.na(title)){
    p <- p + ggplot2::ggtitle(title)
  }

  ### add features
  p <- p+
    ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          color = "black", data = border_lines)+
    ggplot2::scale_size_identity()+
    ggplot2::xlim(0, ntrt)+
    ggplot2::ylim(-0.5, ntrt + 0.5)+
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size=6.7, face = "bold"),
      legend.title = ggplot2::element_text(face = "bold"),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank())


  return(p)
}
