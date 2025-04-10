% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exact_confint.R
\name{exact_confint}
\alias{exact_confint}
\title{Clopper-Pearson Exact Binomial Confidence Interval}
\usage{
exact_confint(
  x,
  n,
  level = 0.95,
  alternative = c("two.sided", "less", "greater"),
  ...
)
}
\arguments{
\item{x}{positive \link[base]{integer} scalar or \link[base]{vector}, counts}

\item{n}{positive \link[base]{integer} scalar or \link[base]{vector}, sample sizes \eqn{n}}

\item{level}{\link[base]{numeric} scalar, confidence level, default .95}

\item{alternative}{\link[base]{character} scalar,
\code{'two.sided'} (default), \code{'less'} or \code{'greater'}}

\item{...}{potential parameters}
}
\value{
Function \link{exact_confint} returns an S3 \code{'exact_confint'} object,
inspired by element \verb{$conf.int} of an \code{'htest'} object,
i.e., the returned value of functions \link[stats]{t.test}, \link[stats]{prop.test}, etc.

An \code{'exact_confint'} object is a \link[base]{double} \link[base]{matrix} with additional \link[base]{attributes},
\describe{
\item{\code{attr(.,'conf.level')}}{\link[base]{double} scalar, default .95, to mimic the element \verb{$conf.int} of an \code{'htest'} object}
\item{\code{attr(.,'alternative')}}{\link[base]{character} scalar}
\item{\code{attr(.,'x')}}{\link[base]{integer} scalar or \link[base]{vector}}
\item{\code{attr(.,'n')}}{\link[base]{integer} scalar or \link[base]{vector}}
}
}
\description{
Clopper-Pearson exact binomial confidence interval.
}
\note{
Function \code{Hmisc::binconf} uses \link[stats]{qf}.

Functions \link[stats]{binom.test} and \code{binom::binom.confint} uses \link[stats]{qbeta} (equivalent but much cleaner!)

Only function \link[stats]{binom.test} provides one-sided confidence interval.
}
\examples{
exact_confint(0:10, 10L)
exact_confint(0:10, 10L, alternative = 'less')
exact_confint(0:10, 10L, alternative = 'greater')
}
\references{
\doi{10.1093/biomet/26.4.404}
}
