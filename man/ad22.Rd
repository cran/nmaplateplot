\name{ad22}
\alias{ad22}
\docType{data}
\title{
Network Meta-Analysis Results on 21 Antidepressants and Placebo
}
\description{
An example of network meta-analysis results.
}
\usage{data("ad22")}
\format{
  A list containing 5 data frames:
  \describe{
    \item{Point_estimates}{a 22*22 data frame storing point estimates.}
    \item{Interval_estimates_LB}{a 22*22 data frame storing lower bound of interval estimates.}
    \item{Interval_estimates_UB}{a 22*22 data frame storing upper bound of interval estimates.}
    \item{Pvalues}{a 22*22 data frame storing p-values.}
	\item{Treatment_specific_values}{a 22*4 data frame storing treatment id, treatment names, treatment surface under the cumulative ranking (SUCRA) for efficacy and acceptability }
  }
}
\details{
It contains odds ratios for efficacy (upper diagonal part) and acceptability (lower diagonal part) of 21 antidepressants and placebo based on contrast-based network meta-analysis
}
\source{
Cipriani A et al. (2018) "Comparative efficacy and acceptability of 21 antidepressant drugs for the acute treatment of adults with major depressive disorder: a systematic review and network meta-analysis." \emph{The Lancet} \bold{391}(10128), 1357--1366.
}
\keyword{datasets}
