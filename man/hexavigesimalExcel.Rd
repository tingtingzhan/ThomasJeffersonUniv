% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Excel2int.R
\name{hexavigesimalExcel}
\alias{hexavigesimalExcel}
\alias{Excel2int}
\alias{Excel2C}
\alias{int2Excel}
\title{Excel-Style Hexavigesimal (A to Z)}
\usage{
Excel2int(x)

Excel2C(x)

int2Excel(x)
}
\arguments{
\item{x}{\link[base]{integer} scalar or \link[base]{vector} for function \link{int2Excel}.
\link[base]{character} scalar or \link[base]{vector} for functions \link{Excel2C} and \link{Excel2int},
which consists of (except missingness)
only letters \code{A} to \code{Z} and \code{a} to \code{z}.}
}
\value{
Function \link{Excel2int} returns an
\link[base]{integer} \link[base]{vector}.

Function \link{Excel2C} returns a
\link[base]{character} \link[base]{vector}.

Function \link{int2Excel} returns a
\link[base]{integer} \link[base]{vector}.
}
\description{
Convert between decimal, C-style hexavigesimal (\code{0} to \code{9}, \code{A} to \code{P}), and Excel-style hexavigesimal (\code{A} to \code{Z}).
}
\details{
Convert between decimal, C-style hexavigesimal, and Excel-style hexavigesimal.

\tabular{lrrrrrrrrrr}{
Decimal \tab 0 \tab 1 \tab 25 \tab 26 \tab 27 \tab 51 \tab 52 \tab 676 \tab 702 \tab 703 \cr
Hexavigesimal; C \tab \code{0} \tab \code{1} \tab \code{P} \tab \code{10} \tab \code{11} \tab \verb{1P} \tab \code{20} \tab \code{100} \tab \code{110} \tab \code{111} \cr
Hexavigesimal; Excel \tab \code{0} \tab \code{A} \tab \code{Y} \tab \code{Z} \tab \code{AA} \tab \code{AY} \tab \code{AZ} \tab \code{YZ} \tab \code{ZZ} \tab \code{AAA} \cr
}

Function \link{Excel2C} converts
Excel-style hexavigesimal (\code{A} to \code{Z})
to C-style hexavigesimal (\code{0} to \code{9}, \code{A} to \code{P}).

Function \link{Excel2int} converts
Excel-style hexavigesimal (\code{A} to \code{Z})
to decimal, using function \link{Excel2C} and \link[base]{strtoi}.

Function \link{int2Excel} converts decimal to
Excel-style hexavigesimal (\code{A} to \code{Z}).
This function works very differently from R's solution to
hexadecimal and decimal conversions. Function \link[base]{as.hexmode} returns an object of \link[base]{typeof} \link[base]{integer}.
Then function \link[base]{format.hexmode}, i.e., the workhorse of function \link[base]{print.hexmode},
relies on \verb{\%x} (hexadecimal) format option of function \link[base]{sprintf}.
}
\examples{
# table in documentation
int1 = c(NA_integer_, 1L, 25L, 26L, 27L, 51L, 52L, 676L, 702L, 703L)
Excel1 = c(NA_character_, 'A', 'Y', 'Z', 'AA', 'AY', 'AZ', 'YZ', 'ZZ', 'AAA')
C1 = c(NA_character_, '1', 'P', '10', '11', '1P', '20', '100', '110', '111')
stopifnot(
 identical(int1, Excel2int(Excel1)), 
 identical(int1, strtoi(C1, base = 26L)),
 identical(int2Excel(int1), Excel1)
)

# another example
int2 = c(NA_integer_, 1L, 4L, 19L, 37L, 104L, 678L)
Excel2 = c(NA_character_, 'a', 'D', 's', 'aK', 'cZ', 'Zb')
Excel2C(Excel2)
stopifnot(
 identical(int2, Excel2int(Excel2)),
 identical(int2Excel(int2), toupper(Excel2))
)
}
\references{
\url{http://mathworld.wolfram.com/Hexavigesimal.html}
}
