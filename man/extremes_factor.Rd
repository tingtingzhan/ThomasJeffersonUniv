% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max.factor.R
\name{extremes_factor}
\alias{extremes_factor}
\alias{max.factor}
\alias{min.factor}
\title{\link[base]{max} and \link[base]{min} of \link[base]{factor}}
\usage{
\method{max}{factor}(..., na.rm = FALSE)

\method{min}{factor}(..., na.rm = FALSE)
}
\arguments{
\item{...}{one \link[base]{factor} object}

\item{na.rm}{\link[base]{logical} scalar}
}
\value{
Functions \link{max.factor} and \link{min.factor} both return a \link[base]{factor}.
}
\description{
..
}
\examples{
(x = structure(c(NA_integer_, sample.int(3L, size = 20L, replace = TRUE)),
 levels = letters[1:3], class = 'factor'))
max(x, na.rm = FALSE)
max(x, na.rm = TRUE)
min(x, na.rm = TRUE)

(x0 = structure(rep(NA_integer_, times = 20L),
 levels = letters[1:3], class = 'factor'))
max(x0)
min(x0)
}
