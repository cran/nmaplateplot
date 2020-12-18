\name{ad12.rr.rd}
\alias{ad12.rr.rd}
\docType{data}
\title{
Network Meta-Analysis Results (RR and RD) on 12 Antidepressants
}
\description{
An example of network meta-analysis results.
}
\usage{data("ad12.rr.rd")}
\format{
  A list containing 5 data frames:
  \describe{
    \item{Point_estimates}{a 12*12 data frame storing point estimates.}
    \item{Interval_estimates_LB}{a 12*12 data frame storing lower bound of interval estimates.}
    \item{Interval_estimates_UB}{a 12*12 data frame storing upper bound of interval estimates.}
    \item{Pvalues}{a 12*12 data frame storing p-values.}
	\item{Treatment_specific_values}{a 12*4 data frame storing treatment id, treatment names, treatment surface under the cumulative ranking (SUCRA)}
  }
}
\details{
It contains results of risk ratios (upper diagonal part) and risk differences (lower diagonal part) for efficacy of 12 antidepressants.
}
\source{
Cipriani A et al. (2009) "Comparative efficacy and acceptability of 12 new-generation antidepressants: a multiple-treatments meta-analysis." \emph{The Lancet} \bold{373}(9665), 746--58.
}
\keyword{datasets}
