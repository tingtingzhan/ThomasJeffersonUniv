


#' @title Proportions of Exceptions
#' 
#' @description ..
#' 
#' @param x ..
#'
#' @param data.name ..
#'  
#' @param ... all potential parameters of \link[base]{as.data.frame}
#' 
#' @details ..
#' 
#' @examples 
#' prop_missing(swiss)
#' prop_missing(airquality)
#' prop_missing(airquality$Ozone)
#' 
#' @name prop_exception
NULL



prop_exception <- function(x, FUN, data.name = stop(), FUN.NAME = deparse1(substitute(FUN)), freq0.rm = TRUE, ...) {
  
  prop_bool <- function(x) {
    if (!length(x)) stop('NULL or len-0 input ?')
    if (!is.logical(x)) stop('Use a function with binary output')
    if (anyNA(x)) stop('do not allow NA in exception detection')
    cbind(sum(x), n = length(x))
  }
  
  if (!is.recursive(x)) {
    ret0 <- prop_bool(FUN(x))
    nm0 <- data.name
  } else {
    y0 <- lapply(x, FUN = \(x) prop_bool(FUN(x)))
    ret0 <- do.call(rbind, args = y0)
    nm0 <- names(x)
  }
  
  nm1 <- substr(nm0, start = 1L, stop = 30L)
  
  if (!length(id <- which(ret0[,1L] > 0L))) {
    message(c(sQuote(data.name), ' does not have columns with ', dQuote(FUN.NAME), ' element'))
    return(invisible())
  }
  
  if (freq0.rm) {
    x <- ret0[id, 1L]
    nm <- nm1[id]
    n <- ret0[id, 2L]
  } else {
    x <- ret0[, 1L]
    nm <- nm1
    n <- ret0[, 2L]
  }
  
  return(noquote(array(c(
    sprintf(fmt = '%d / %d', x, n),
    sprintf(fmt = '%.1f%%', 1e2*x/n)
  ), dim = c(length(x), 2L), dimnames = list(
    nm,
    c(FUN.NAME, 'Percentage')
  ))))
  
}


#' @rdname prop_exception
#' @export
prop_missing <- function(x, data.name = deparse1(substitute(x)), ...) {
  prop_exception(x, FUN = is.na, data.name = data.name, FUN.NAME = c('Missing'), ...)
}


#' @rdname prop_exception
#' @export
prop_nonmissing <- function(x, data.name = deparse1(substitute(x)), ...) {
  prop_exception(x, FUN = \(x) !is.na(x), data.name = data.name, FUN.NAME = c('Non-Missing'), ...)
}

