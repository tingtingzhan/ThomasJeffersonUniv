% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchDF.R
\name{rsplit_}
\alias{rsplit_}
\title{Split \link[base]{data.frame} into Individual Rows}
\usage{
rsplit_(x)
}
\arguments{
\item{x}{\link[base]{data.frame}}
}
\value{
Function \code{\link[=rsplit_]{rsplit_()}} returns a \link[base]{list} of \link[base]{nrow}-1 \link[base]{data.frame}s.
}
\description{
\link[base]{split.data.frame} into individual rows.
}
\examples{
mtcars |> head(n = 3L) |> rsplit_() # with rownames
Formaldehyde |> rsplit_() # without rownames
data.frame() |> rsplit_() # exception

}
\keyword{internal}
