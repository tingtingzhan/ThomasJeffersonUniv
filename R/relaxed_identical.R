

#' @title Relaxed Identical
#' 
#' @description
#' Slightly relax the \link[base]{identical} criteria, 
#' to identify near-identical objects, 
#' e.g., model estimates with mathematically identical model specifications.
#' 
#' @param x,y any **R** objects
#' 
#' @details
#' Function [relaxed_identical] relaxes function \link[base]{identical} in the following ways.
#' 
#' ## For object with \link[base]{typeof} being \link[base]{double} and/or \link[base]{integer}
#' 
#' Test near equality with \link[base]{attributes} ignored, 
#' i.e., function \link[base]{all.equal.numeric} with option `check.attributes = FALSE`.
#'
#' ## For \link[stats]{formula}s
#' 
#' Set \link[base]{environment} of `x` and `y` to `NULL`, 
#' then compare them using function \link[base]{identical}.
#' Note that 
#' \itemize{
#' \item{\link[stats]{formula} \link[base]{is.recursive}, thus must be placed before the \link[base]{is.recursive} branching;}
#' \item{\link[stats]{formula} is *not* \link[base]{closure}.
#' Therefore, using function \link[base]{identical} with option `ignore.environment = TRUE` does *not* work!}
#' }
#' 
#' ## For \link[base]{function}s
#' 
#' Ignore \link[base]{environment} of `x` and `y`, 
#' i.e., using function \link[base]{identical} with option `ignore.environment = TRUE`.  
#' Note that 
#' \itemize{
#' \item{\link[base]{function} \link[base]{is.recursive}, whether it is \link[base]{closure} or \link[base]{is.primitive}.
#' Therefore it must be placed before the \link[base]{is.recursive} branching;} 
#' }
#' 
#' ## For all other \link[base]{is.recursive} objects
#' 
#' Function [relaxed_identical] is called ***recursively***, for each \link[base]{$} element of `x` and `y`.
#' 
#' ## For \link[base]{S4} objects
#' Function [relaxed_identical] is called ***recursively***, for each \link[base]{@@} \link[methods]{slot} 
#' (which is technically the \link[base]{attributes}) of `x` and `y`, 
#' including the `@.Data` slot.
#' Note that 
#' \itemize{
#' \item \link[base]{S4} objects are ***not*** \link[base]{is.recursive}.
#' }
#' 
#' 
#' ## Otherwise
#' Function \link[base]{identical} is called, as the exception handling.
#' 
#' @returns 
#' Function [relaxed_identical] returns a \link[base]{logical} scalar.
#' 
#' @examples
#' # mathematically identical model specification
#' m1 = lm(breaks ~ -1 + wool + wool:tension, data = warpbreaks)
#' m2 = lm(breaks ~ -1 + tension + tension:wool, data = warpbreaks)
#' foo = function(m) list(pred = predict(m), resid = residuals(m))
#' identical(foo(m1), foo(m2)) # FALSE
#' relaxed_identical(foo(m1), foo(m2)) # TRUE
#' @export
relaxed_identical <- function(x, y) {
  
  if ((typeof(x) %in% c('integer', 'double')) && (typeof(y) %in% c('integer', 'double'))) {
    return(isTRUE(all.equal.numeric(
      target = x, current = y, 
      check.attributes = FALSE
    )))
  }
  
  if (inherits(x, what = 'formula') && inherits(y, what = 'formula')) {
    if (identical(x, y)) return(TRUE)
    environment(x) <- environment(y) <- NULL
    return(identical(x, y))
    # return(identical(x, y, ignore.environment = TRUE)) # wrong!! formula not \link[base]{closure}
  }

  if (inherits(x, what = 'function') && inherits(y, what = 'function')) {
    return(identical(x, y, ignore.environment = TRUE))
  }
  
  if (is.recursive(x) && is.recursive(y)) {
    if (length(x) != length(y)) return(FALSE)
    return(all(mapply(FUN = relaxed_identical, x, y, SIMPLIFY = TRUE)))
  } # recursive call; beautiful!!
  
  if (isS4(x) && isS4(y)) {
    return(relaxed_identical(x@.Data, y@.Data) & relaxed_identical(attributes(x), attributes(y)))
  } # recursive call on attributes (not tested, but should be correct)
  
  return(identical(x, y)) # exception handling
  
}