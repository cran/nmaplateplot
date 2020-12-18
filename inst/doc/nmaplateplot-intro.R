## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = TRUE)
knitr::opts_chunk$set(out.width = "1200px", dpi = 300, fig.height = 6, fig.width = 10,
                      dev = "svglite", fig.align = "center",
                      out.extra = 'style="display:block; margin: auto"')

## -----------------------------------------------------------------------------
library(nmaplateplot)
library(svglite)
data("ad12.eff.acc")
plateplot(ad12.eff.acc, design_method = c("circle", "circle"),
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
# option design_method controls whether you want upper and lower diagonal parts to display "text" or "circle" respectively.
plateplot(ad12.eff.acc, design_method = c("text", "text"),
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
nma_res_minimum <- ad12.eff.acc
nma_res_minimum$Treatment_specific_values$Value_Upper <- NULL
nma_res_minimum$Treatment_specific_values$Value_Lower <- NULL
# This is the minimum input for the plateplot function
nma_res_minimum$Treatment_specific_values
round(nma_res_minimum$Point_estimates,2)
round(nma_res_minimum$Interval_estimates_LB,2)
round(nma_res_minimum$Interval_estimates_UB,2)
round(nma_res_minimum$Pvalues,3)
# The text output based on minimum input
plateplot(nma_res_minimum, design_method = c("text", "text"),
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
plateplot(nma_res_minimum, design_method = c("text", "text"),
             transform_rc_ullr_boolean = FALSE,
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
plateplot(nma_res_minimum, design_method = c("text", "text"),
             transform_rc_ullr_boolean = TRUE,
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
# only show SUCRA for lower diagonal part
nma_res_SUCRA_LD <- ad12.eff.acc
nma_res_SUCRA_LD$Treatment_specific_values$Value_Lower <- NULL
plateplot(nma_res_SUCRA_LD, design_method = c("circle", "circle"),
             transform_rc_ullr_boolean = TRUE,
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

# only show SUCRA for upper diagonal part
nma_res_SUCRA_UD <- ad12.eff.acc
nma_res_SUCRA_UD$Treatment_specific_values$Value_Upper <- NULL
plateplot(nma_res_SUCRA_UD, design_method = c("circle", "circle"),
             transform_rc_ullr_boolean = TRUE,
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

# show both, but they are different
nma_res_SUCRA_Bothd <- ad12.eff.acc
plateplot(nma_res_SUCRA_Bothd, design_method = c("circle", "circle"),
             transform_rc_ullr_boolean = TRUE,
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

# show both and they are same
nma_res_SUCRA_Boths <- ad12.eff.acc
nma_res_SUCRA_Boths$Treatment_specific_values$Value_Lower <- 
  nma_res_SUCRA_Boths$Treatment_specific_values$Value_Upper
plateplot(nma_res_SUCRA_Boths, design_method = c("circle", "circle"),
             transform_rc_ullr_boolean = TRUE,
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## ----eval=FALSE---------------------------------------------------------------
#  nma_res_minimum <- plateplot(nma_res_minimum, design_method = c("text", "text"),
#                                  upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")
#  ggsave("nma_res_minimum.pdf", plot = nma_res_minimum, device = "pdf",
#         width = 10, height = 6, dpi = 600)
#  ggsave("nma_res_minimum.tiff", plot = nma_res_minimum, device = "tiff", compression = "lzw",
#         width = 10, height = 6, dpi = 600)
#  ggsave("nma_res_minimum.svg", plot = nma_res_minimum, device = "svg",
#         width = 10, height = 6, dpi = 600)

## -----------------------------------------------------------------------------
data("ad12.rr.rd")
plateplot(ad12.rr.rd,
             null_value_zero = c(FALSE, TRUE), lower_better = c(FALSE, FALSE),
             design_method = c("text", "text"), 
             upper_diagonal_name = "Efficacy: Risk ratio", lower_diagonal_name = "Efficacy: Risk difference")

data("ad12.eff.acc")
plateplot(ad12.eff.acc, 
             null_value_zero = c(FALSE, FALSE), lower_better = c(FALSE, TRUE),
             design_method = c("text", "text"),
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
data("ad12.eff.acc")
ad12.eff.acc$Treatment_specific_values$Order <- c(1:6,12:7)
plateplot(ad12.eff.acc, 
             null_value_zero = c(FALSE, FALSE), lower_better = c(FALSE, TRUE),
             design_method = c("circle", "circle"),
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
data("ad12.eff.acc")
ad12.eff.acc$Symbol_indicators <- as.data.frame(matrix(NA, nrow = 12, ncol = 12))
plateplot(ad12.eff.acc, 
             null_value_zero = c(FALSE, FALSE), lower_better = c(FALSE, TRUE),
             design_method = c("text", "text"),
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
data("ad12.eff.acc")
plateplot(ad12.eff.acc, design_method = c("circle", "circle"), 
             plate_circle_minsize = c(1.0, 1.0), plate_circle_maxsize = c(8.0, 8.0), 
             plate_circle_samesize = FALSE,
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
plateplot(ad12.rr.rd,
             null_value_zero = c(FALSE, TRUE), lower_better = c(FALSE, FALSE),
             design_method = c("text", "text"), text_size = 2.0,
             upper_diagonal_name = "Efficacy: Risk ratio", lower_diagonal_name = "Efficacy: Risk difference")

## -----------------------------------------------------------------------------
plateplot(ad12.rr.rd,
             null_value_zero = c(FALSE, TRUE), lower_better = c(FALSE, FALSE),
             design_method = c("text", "text"), text_size = 2.8, bold = TRUE,
             upper_diagonal_name = "Efficacy: Risk ratio", lower_diagonal_name = "Efficacy: Risk difference")

## -----------------------------------------------------------------------------
plateplot(ad12.eff.acc, design_method = c("circle", "circle"), max_substring = 2,
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
plateplot(ad12.eff.acc, design_method = c("circle", "circle"), 
             diagonal_color = c("white", "green3"),
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
plateplot(ad12.eff.acc, design_method = c("circle", "circle"), 
             text_and_circle_color = c("blue", "white", "red", "grey70", "black"),
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")
plateplot(ad12.eff.acc, design_method = c("text", "text"), 
             text_and_circle_color = c("red", "black", "blue"),
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
plateplot(ad12.eff.acc, design_method = c("circle", "circle"), 
             offdiagonal_color = c("cornsilk1", "white"),
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
plateplot(ad12.eff.acc, design_method = c("circle", "circle"), 
             title = "Efficacy and acceptability of 12 antidepressants",
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

## -----------------------------------------------------------------------------
data("ad12.pma.nma")
plateplot(ad12.pma.nma, design_method = c("circle", "circle"), 
             plate_circle_minsize = c(2, 2), plate_circle_maxsize = c(30, 30), 
             plate_circle_samesize = TRUE,
             upper_diagonal_name = "Network meta-analysis", lower_diagonal_name = "Pairwise meta-analysis")
plateplot(ad12.pma.nma, design_method = c("circle", "text"), 
             upper_diagonal_name = "Network meta-analysis", lower_diagonal_name = "Pairwise meta-analysis")

## -----------------------------------------------------------------------------
data("ad22")
plateplot(ad22, design_method = c("circle", "circle"), 
             plate_circle_minsize = c(1.5, 1.5), plate_circle_maxsize = c(7, 7), 
             text_size = 1.5, plate_circle_samesize = FALSE,
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

plateplot(ad22, design_method = c("text", "text"), text_size = 1.5,
             upper_diagonal_name = "Efficacy", lower_diagonal_name = "Acceptability")

