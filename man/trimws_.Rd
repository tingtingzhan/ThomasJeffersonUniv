% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trimws_.R
\name{trimws_}
\alias{trimws_}
\title{Remove Leading/Trailing and Duplicated (Symbols that Look Like) White Spaces}
\usage{
trimws_(x)
}
\arguments{
\item{x}{an object with \link[base]{typeof} being \link[base]{character}}
}
\value{
Function \code{\link[=trimws_]{trimws_()}} returns an object of \link[base]{typeof} \link[base]{character}.
}
\description{
To remove leading/trailing and duplicated (symbols that look like) white spaces.

More aggressive than function \link[base]{trimws}.
}
\details{
Function \code{\link[=trimws_]{trimws_()}} is more aggressive than \link[base]{trimws}, that it removes

\itemize{
\item {non-UTF-8 characters}
\item {duplicated white spaces}
\item {symbols that look like white space, such as \verb{\\u00a0} (no-break space)}
}
}
\note{
\link[base]{gsub} keeps \link[base]{attributes}
}
\examples{
(x = c(A = ' a  b  ', b = 'a .  s', ' a  ,  b ; ', '\u00a0  ab '))
base::trimws(x)
trimws_(x)

cat(x0 <- ' ab  \xa0cd ')
tryCatch(base::trimws(x0), error = identity)
trimws_(x0)

}
\keyword{internal}
