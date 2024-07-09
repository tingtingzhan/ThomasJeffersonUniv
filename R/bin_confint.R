

#' @title Clopper-Pearson Exact Binomial Confidence Interval
#' 
#' @description 
#' Clopper-Pearson exact binomial confidence interval.
#' 
#' @param x positive \link[base]{integer} \link[base]{vector}, counts
#' 
#' @param n positive \link[base]{integer} \link[base]{vector}, sample sizes \eqn{n}
#' 
#' @param level \link[base]{numeric} scalar, confidence level, default .95
#' 
#' @param alternative \link[base]{character} scalar,
#' `'two.sided'` (default), `'less'` or `'greater'`
#' 
#' @param ... potential parameters
#' 
#' @returns 
#' 
#' Function [bin_confint_] returns a \link[base]{matrix}.
#' 
#' @note  
#' 
#' Function `Hmisc::binconf` uses \link[stats]{qf}.
#' 
#' \link[stats]{binom.test} and `binom::binom.confint` uses \link[stats]{qbeta} (equivalent but much cleaner!)
#' 
#' Only \link[stats]{binom.test} provides one-sided confidence interval.
#' 
#' @references 
#' \doi{10.1093/biomet/26.4.404}
#' 
#' @examples 
#' bin_confint_(0:10, 10L)
#' bin_confint_(0:10, 10L, alternative = 'less')
#' bin_confint_(0:10, 10L, alternative = 'greater')
#' 
#' @importFrom stats qbeta
#' @export
bin_confint_ <- function(x, n, level = .95, alternative = c('two.sided', 'less', 'greater'), ...) {
  
  if (!is.integer(x) || !length(x) || anyNA(x)) stop('x must be integer')
  if (!is.integer(n) || !length(n) || anyNA(n) || any(n < 0L)) stop('n must be non-negative integer')
  if (!is.double(level) || length(level) != 1L || anyNA(level) || level < 0 || level > 1) stop('illegal level')
  
  tmp <- cbind(x, n) # vector recycling, let warn
  x <- tmp[,1L]
  n <- unname(tmp[,2L])
  nx <- length(x)
  if (any(x < 0L, n < x)) stop('x must be non-negative and less than `n`')
  
  alternative <- match.arg(alternative)
  switch(alternative, two.sided = {
    aL <- (1-level)/2
    aU <- 1 - aL
  }, less = {
    aL <- 0
    aU <- level
  }, greater = {
    aL <- 1 - level
    aU <- 1
  })
  
  ret <- array(rep(c(0,1), each = nx), dim = c(nx, 2L), dimnames = list(rownames(tmp), sprintf(fmt = '%.1f%%', 1e2*c(aL, aU))))
  
  switch(alternative, two.sided =, greater = {
    id1 <- (x > 0L)
    ret[id1, 1L] <- qbeta(aL, shape1 = x[id1], shape2 = (n-x+1L)[id1]) # otherwise (i.e. x == 0L) still 0
  })
  switch(alternative, two.sided =, less = {
    id2 <- (x < n)
    ret[id2, 2L] <- qbeta(aU, shape1 = (x+1L)[id2], shape2 = (n-x)[id2]) # otherwise (i.e. x == n) still 1
  })
  
  attr(ret, which = 'x') <- x
  attr(ret, which = 'n') <- n
  attr(ret, which = 'level') <- level
  attr(ret, which = 'alternative') <- alternative
  return(ret)
  
}





format_bin_confint_ <- function(..., count.name) {
  
  cint <- bin_confint_(...)
  x <- attr(cint, which = 'x', exact = TRUE)
  n <- attr(cint, which = 'n', exact = TRUE)
  level <- attr(cint, which = 'level', exact = TRUE)
  alternative <- attr(cint, which = 'alternative', exact = TRUE)
  
  p_ <- sprintf(fmt = '%.1f%% (%.1f%%, %.1f%%)', 1e2*x/n, 1e2*cint[,1L], 1e2*cint[,2L])
  p_[n == 0L] <- '-'
  if (missing(count.name)) return(p_)
  
  if (!is.character(count.name) || length(count.name) != 1L || is.na(count.name)) stop('illegal `count.name`')
  ret <- cbind(sprintf(fmt = '%d / %d', x, n), p_)
  dimnames(ret) <- list(rownames(cint), c(count.name, 
    sprintf(fmt = 'Percentage (%.f%% %s-Sided Exact CI)', 1e2*level, switch(alternative, two.sided = '2', '1'))
  ))
  return(noquote(ret, right = TRUE))
  
}



