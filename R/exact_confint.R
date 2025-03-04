

#' @title Clopper-Pearson Exact Binomial Confidence Interval
#' 
#' @description 
#' Clopper-Pearson exact binomial confidence interval.
#' 
#' @param x positive \link[base]{integer} scalar or \link[base]{vector}, counts
#' 
#' @param n positive \link[base]{integer} scalar or \link[base]{vector}, sample sizes \eqn{n}
#' 
#' @param level \link[base]{numeric} scalar, confidence level, default .95
#' 
#' @param alternative \link[base]{character} scalar,
#' `'two.sided'` (default), `'less'` or `'greater'`
#' 
#' @param ... potential parameters
#' 
#' @returns 
#' Function [exact_confint] returns an S3 `'exact_confint'` object,
#' inspired by element `$conf.int` of an `'htest'` object,
#' i.e., the returned value of functions \link[stats]{t.test}, \link[stats]{prop.test}, etc.
#' 
#' An `'exact_confint'` object is a \link[base]{double} \link[base]{matrix} with additional \link[base]{attributes},
#' \describe{
#' \item{`attr(.,'conf.level')`}{\link[base]{double} scalar, default .95, to mimic the element `$conf.int` of an `'htest'` object}
#' \item{`attr(.,'alternative')`}{\link[base]{character} scalar}
#' \item{`attr(.,'x')`}{\link[base]{integer} scalar or \link[base]{vector}}
#' \item{`attr(.,'n')`}{\link[base]{integer} scalar or \link[base]{vector}}
#' }
#' 
#' @note  
#' 
#' Function `Hmisc::binconf` uses \link[stats]{qf}.
#' 
#' Functions \link[stats]{binom.test} and `binom::binom.confint` uses \link[stats]{qbeta} (equivalent but much cleaner!)
#' 
#' Only function \link[stats]{binom.test} provides one-sided confidence interval.
#' 
#' @references 
#' \doi{10.1093/biomet/26.4.404}
#' 
#' @examples 
#' exact_confint(0:10, 10L)
#' exact_confint(0:10, 10L, alternative = 'less')
#' exact_confint(0:10, 10L, alternative = 'greater')
#' @importFrom stats qbeta
#' @export
exact_confint <- function(x, n, level = .95, alternative = c('two.sided', 'less', 'greater'), ...) {
  
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
  attr(ret, which = 'conf.level') <- level
  attr(ret, which = 'alternative') <- alternative
  class(ret) <- 'exact_confint'
  return(ret)
  
}



#' @title Format Clopper-Pearson Exact Binomial Confidence Interval
#' 
#' @description
#' To \link[base]{format} a Clopper-Pearson exact binomial confidence interval.
#' 
#' @param x an [exact_confint]
#' 
#' @param data.name (optional) \link[base]{character} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function [format.exact_confint] returns a \link[base]{character}
#' \link[base]{vector} when argument `data.name` is \link[base]{missing};
#' otherwise, a \link[base]{noquote} \link[base]{character} \link[base]{matrix} is returned.
#' 
#' @export format.exact_confint
#' @export
format.exact_confint <- function(x, data.name, ...) {
  
  object <- x; x <- NULL
  x <- attr(object, which = 'x', exact = TRUE)
  n <- attr(object, which = 'n', exact = TRUE)
  level <- attr(object, which = 'conf.level', exact = TRUE)
  alternative <- attr(object, which = 'alternative', exact = TRUE)
  
  p_ <- sprintf(fmt = '%.1f%% (%.1f%%, %.1f%%)', 1e2*x/n, 1e2*object[,1L], 1e2*object[,2L])
  p_[n == 0L] <- '-'
  if (missing(data.name)) return(p_)
  
  if (is.language(data.name)) data.name <- deparse1(data.name)
  if (!is.character(data.name) || length(data.name) != 1L || is.na(data.name)) stop('illegal `data.name`')
  ret <- cbind(sprintf(fmt = '%d / %d', x, n), p_)
  dimnames(ret) <- list(rownames(object), c(data.name, 
    sprintf(fmt = 'Percentage (%.f%% %s-Sided Exact CI)', 1e2*level, switch(alternative, two.sided = '2', '1'))
  ))
  return(noquote(ret, right = TRUE))
  
}

#' @export
print.exact_confint <- function(x, data.name = substitute(x), ...) {
  format.exact_confint(x, data.name = data.name, ...) |> print()
}




