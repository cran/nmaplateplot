\name{plateplot}
\alias{plateplot}
\title{
Plotting the NMA results
}
\description{
\code{plateplot} plots a graphical display of results from network meta-analysis (NMA)
}
\usage{
plateplot(nma_result, 
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
          text_and_circle_color = c("red", "grey10", "blue", "grey70", "white"))
}
\arguments{
  \item{nma_result}{
  the network meta-analysis results. Please load attached datasets "Antidepressants12_1" for details.
}
  \item{null_value_zero}{
  a vector of two logical values for upper and lower diagonal parts respectively. TRUE indicates the null value of estimates is zero, and FALSE indicates the null value of estimates is one. Obviously, we should use FALSE and TRUE for risk ratio and odds ratio, and choose TRUE for risk difference and standardized mean difference.
}
  \item{lower_better}{
  a vector of two logical values for upper and lower diagonal parts respectively. TRUE indicates lower estimates implying better treatment, and vice versa.
  
}
  \item{design_method}{
  a vector of indicators for upper diagonal matrix and lower diagonal matrix. design_method = "cicle" means plate plot is drawn; design_method = "text" means text is displayed. Allowed values are c(upper diagonal matrix, lower diagonal matrix): c("circle", "text"), c("circle", "circle"), c("text", "circle"), c("text", "text").
}
  \item{plate_circle_minsize}{
  a vector of circle minimum size for upper diagonal matrix and lower diagonal parts.
}
  \item{plate_circle_maxsize}{
  a vector of circle maximum size for upper diagonal matrix and lower diagonal parts.
}
  \item{plate_circle_samesize}{
  a logical value. If you think upper and lower diagonal need to have the same function to adjust the circle size, you can set plate_circle_samesize = TRUE.
}
  \item{transform_rc_ullr_boolean}{
  a logical value to indicate whether you want to transform the dataset from row-column type to upper-left lower-right type.
}
  \item{text_size}{
  a positive real number for text size in the plot.
}
  \item{bold}{
  a logical value to indicate whether you want numbers to be bold or not.
}
  \item{max_substring}{
  a positive number to determine to display first several (max_substring) characters of treatment names.
}
  \item{title}{
  a character string fot the title.
}
  \item{upper_diagonal_name}{
  a character string for the name of upper diagonal part.
}
  \item{lower_diagonal_name}{
  a character string for the name of lower diagonal part.
}
  \item{diagonal_color}{
  an array of two color names to define colors for SUCRA=0 and SUCRA=1.
}
  \item{offdiagonal_color}{
  an array of two colors for upper and lower diagonal parts.
}
  \item{text_and_circle_color}{
  an array of five color names (A, B, C, D, E). “(A, B, C)” is used to define varying colors (based on low, mid and high p-values) for texts or circles represent upper bound of interval estimate. “D” is color for circles represent point estimate. “E” is color for circles represent lower bound of interval estimate.
}
}

\value{
A ggplot object is generated.
}

\details{
Please read package's Vignette for details.
}

\examples{
data("ad12.eff.acc")
plateplot(ad12.eff.acc, 
	design_method = c("circle", "circle"),
	upper_diagonal_name = "Efficacy", 
	lower_diagonal_name = "Acceptability")
plateplot(ad12.eff.acc, 
	design_method = c("text", "text"),
	upper_diagonal_name = "Efficacy",
	lower_diagonal_name = "Acceptability")
	
data("ad12.rr.rd")
plateplot(ad12.rr.rd,
	null_value_zero = c(FALSE, TRUE), 
	lower_better = c(FALSE, FALSE),
	design_method = c("text", "text"), 
	text_size = 2.8, bold = TRUE,
	upper_diagonal_name = "Efficacy: Risk ratio", 
	lower_diagonal_name = "Efficacy: Risk difference")
	
data("ad12.pma.nma")
plateplot(ad12.pma.nma, 
	design_method = c("circle", "circle"), 
	plate_circle_minsize = c(2, 2), 
	plate_circle_maxsize = c(30, 30), 
	plate_circle_samesize = TRUE,
	upper_diagonal_name = "Network meta-analysis", 
	lower_diagonal_name = "Pairwise meta-analysis")
	
data("ad22")
plateplot(ad22, 
	design_method = c("circle", "circle"), 
	plate_circle_minsize = c(1.5, 1.5), 
	plate_circle_maxsize = c(7, 7), 
	text_size = 1.5, 
	plate_circle_samesize = FALSE,
	upper_diagonal_name = "Efficacy", 
	lower_diagonal_name = "Acceptability")			 
}