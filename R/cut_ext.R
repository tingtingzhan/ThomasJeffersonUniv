
#' @title Divide Numeric Into Intervals
#' 
#' @description 
#' Divide numeric into intervals; an alternative to function \link[base]{cut.default}.
#' 
#' @param x \link[base]{integer}, \link[base]{numeric}, \link[base]{difftime}, \link[base]{Date},
#' \link[base]{POSIXct} \link[base]{vector} or \link[base]{matrix}.
#' 
#' @param breaks \link[base]{vector} of the same \link[base]{class} as `x`. 
#' `-Inf` and `Inf` will be automatically added
#' 
#' @param right \link[base]{logical} scalar, default `TRUE`, 
#' see functions \link[base]{cut.default} and \link[base]{.bincode}.
#' 
#' @param include.lowest \link[base]{logical} scalar, default `TRUE`, 
#' see functions \link[base]{cut.default} and \link[base]{.bincode}.
#' 
#' @param data.name \link[base]{character} scalar, name of data.
#' R \link[base]{language} is also accepted.
#' Default is the argument call of `x`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details 
#' 
#' Function [cut_] is different from function \link[base]{cut.default}, that 
#' \itemize{
#' \item {More classes of `x` are accepted, see **Arguments**}
#' \item {`-Inf` and `Inf` are added to `breaks`, 
#' so that the values outside of `breaks` will be correctly categorized,
#' instead of returning an `NA_integer` per \link[base]{.bincode}}
#' \item {More user-friendly \link[base]{factor} \link[base]{levels}, see helper function [cut_levels]}
#' }
#' 
#' 
#' 
#' @examples 
#' x = c(tmp <- c(10, 31, 45, 50, 52, NA, 55, 55, 57, 58.5, 60, 92), rev.default(tmp))
#' (xm = array(x, dim = c(6L, 4L)))
#' brk = c(20, 60, 80)
#' cut_(x, breaks = brk)
#' cut_(xm, breaks = brk)
#' 
#' (x2 = zoo::as.Date.ts(airmiles))
#' length(x2)
#' cut_(x2, breaks = as.Date(c('1942-01-01', '1950-01-01')))
#' 
#' x2a = x2
#' attr(x2a, 'dim') = c(4L, 6L)
#' cut_(x2a, breaks = as.Date(c('1942-01-01', '1950-01-01')))
#' 
#' x3 = 0:10
#' cut_(x3, breaks = c(0, 3, 6), right = FALSE)
#' cut_(x3, breaks = c(0, 3, 6), right = TRUE)
#' 
#' if (FALSE) {
#' # ?base::.bincode much faster than ?base::findInterval
#' x = 2:18
#' v = c(5, 10, 15) # create two bins [5,10) and [10,15)
#' findInterval(x, v)
#' .bincode(x, v)
#' library(microbenchmark)
#' microbenchmark(findInterval(x, v), .bincode(x, v))
#' }
#'  
#' @name cut_ext
#' @export
cut_ <- function(
    x, 
    breaks,
    right = TRUE,
    include.lowest = TRUE,
    data.name = substitute(x),
    ...
) {
  
  if (inherits(x, what = 'POSIXlt')) x <- as.POSIXct.POSIXlt(x)
  if (is.array(x)) {
    if (typeof(x) != 'double') stop('`x` must be typeof-\'double\' matrix')
  } else if (!inherits(x, what = c('integer', 'numeric', 'difftime', 'Date', 'POSIXct'))) {
    stop('illegal `x` ', sQuote(class(x)[1L]))
  }
  
  breaks <- sort(unique(c(breaks, -Inf, Inf))) 
  # `breaks` before `Inf` and `-Inf`, then the returned value has attributes of `breaks`
  # *not* ?base::sort.int # I need to retain the class of `breaks`
  # dont want to include my [unique_allequal]
  
  ret <- .bincode(x, breaks = breaks, right = right, include.lowest = include.lowest) # 'integer'
  if (is.array(x)) {
    attributes(ret)[c('dim', 'dimnames')] <- attributes(x)[c('dim', 'dimnames')]
  }
  structure(ret, class = 'factor',
            levels = cut_levels(breaks = breaks, right = right, include.lowest = include.lowest, data.name = data.name))

}






#' @rdname cut_ext
#' @examples 
#' ## Examples on Helper Function cut_levels()
#' foo = function(...) cbind(
#'  levels(cut.default(numeric(0), ...)),
#'  cut_levels(...)
#' )
#' foo(breaks = 1:4, right = TRUE, include.lowest = TRUE)
#' foo(breaks = 1:4, right = FALSE, include.lowest = TRUE)
#' foo(breaks = 1:4, right = TRUE, include.lowest = FALSE)
#' foo(breaks = 1:4, right = FALSE, include.lowest = FALSE)
#' set.seed(2259); foo(breaks = c(-Inf, sort(rnorm(1:3)), Inf))
#' @export
cut_levels <- function(
    breaks, 
    right = TRUE, 
    include.lowest = TRUE, 
    data.name = 'x'
) {
  if (is.language(data.name)) data.name <- deparse1(data.name)
  if (!is.character(data.name) || length(data.name) != 1L || is.na(data.name) || !nzchar(data.name)) stop('illegal `data.name`')
  nb <- length(breaks)
  
  fmt_breaks <- trimws(if (inherits(breaks, what = 'Date')) {
    format(breaks) # use S3
  } else if (is.integer(breaks)) {
    as.character.default(breaks)
  } else if (is.double(breaks)) {
    sprintf(fmt = '%.3g', breaks)
  })
  b1 <- fmt_breaks[1:(nb-1L)]
  b2 <- fmt_breaks[2:nb]
  
  if (right) {
    # `r`elationship
    r1 <- if (!include.lowest) '<' else c('\u2264', rep('<', times = nb - 2L)) 
    r2 <- '\u2264'
  } else {
    r1 <- '\u2264'
    r2 <- if (!include.lowest) '<' else c(rep('<', times = nb - 2L), '\u2264') 
  }
  
  L <- gsub(pattern = '^-Inf\u2264|^-Inf<', replacement = '', x = paste0(b1, r1)) 
  R <- gsub(pattern = '\u2264Inf$|<Inf$', replacement = '', x = paste0(r2, b2)) 
  
  if (!nzchar(R[nr <- length(R)])) {
    # change last category from `a<X` or `a<=X` to `X>a` and `X>=a`
    L[nr] <- ''
    R[nr] <- if (identical(r1[length(r1)], '\u2264')) {
      paste0('\u2265', b1[nr])
    } else if (identical(r1[length(r1)], '<')) {
      paste0('>', b1[nr])
    } else stop('shouldnt come here')
  }
  
  paste0(L, data.name, R)
}


