
# does not work!!! why???
#setOldClass(Classes = 'Surv')


# @title Surv Over
# 
# @param e1 ..
# 
# @param e2 ..
# 
# @export
#setMethod(`>`, signature = signature(e1 = 'Surv', e2 = 'numeric'), definition = function(e1, e2) {
#  cat('abc')
#})

#' @title \link[survival]{Surv}ival Over A Time
#' 
#' @param x a \link[survival]{Surv} object
#' 
#' @param tm \link[base]{numeric} scalar
#' 
#' @param censored \link[base]{logical} scalar, 
#' returned value for censored subjects
#' 
#' @returns 
#' Function [survOver] returns a \link[base]{logical} \link[base]{vector}.
#' 
#' @examples
#' library(survival)
#' (x = Surv(1:4, event = c(0,1,0,1)))
#' survOver(x, tm = 3)
#' survOver(x, tm = 3, censor = FALSE)
#' @export
survOver <- function(x, tm, censored = NA) {
  if (!inherits(x, 'Surv') || ncol(x) != 2L) stop('`x` must be right censored')
  if (!is.numeric(tm) || length(tm) != 1L || is.na(tm)) stop('`tm` must be len-1 numeric')
  if (isTRUE(censored)) warning('`censored` should be NA or FALSE')
  
  ret <- (x[,1L] >= tm)
  ret[x[,1L] < tm & x[,2L] == 0] <- censored # censored before `tm`
  return(ret)
}


