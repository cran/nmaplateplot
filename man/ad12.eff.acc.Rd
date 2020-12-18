\name{ad12.eff.acc}
\alias{ad12.eff.acc}
\docType{data}
\title{
Network Meta-Analysis Results (Efficacy and Acceptability) on 12 Antidepressants
}
\description{
An example of network meta-analysis results.
}
\usage{data("ad12.eff.acc")}
\format{
  A list containing 5 data frames:
  \describe{
    \item{Point_estimates}{a 12*12 data frame storing point estimates.}
    \item{Interval_estimates_LB}{a 12*12 data frame storing lower bound of interval estimates.}
    \item{Interval_estimates_UB}{a 12*12 data frame storing upper bound of interval estimates.}
    \item{Pvalues}{a 12*12 data frame storing p-values.}
	\item{Treatment_specific_values}{a 12*4 data frame storing treatment id, treatment names, treatment surface under the cumulative ranking (SUCRA) for efficacy and acceptability }
  }
}
\details{
It contains odds ratios for efficacy (upper diagonal part) and acceptability (lower diagonal part) of 12 antidepressants based on arm-based network meta-analysis
}
\source{
Cipriani A et al. (2009) "Comparative efficacy and acceptability of 12 new-generation antidepressants: a multiple-treatments meta-analysis." \emph{The Lancet} \bold{373}(9665), 746--58.
}
\keyword{datasets}
