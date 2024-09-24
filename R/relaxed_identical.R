

#' @title Relaxed Identical
#' 
#' @description
#' Slightly relax the \link[base]{identical} criteria, 
#' to identify near-identical objects, 
#' e.g., model estimates with mathematically identical model specifications.
#' 
#' @param e1,e2 any **R** objects
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
#' Set \link[base]{environment} of `e1` and `e2` to `NULL`, 
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
#' Ignore \link[base]{environment} of `e1` and `e2`, 
#' i.e., using function \link[base]{identical} with option `ignore.environment = TRUE`.  
#' Note that 
#' \itemize{
#' \item{\link[base]{function} \link[base]{is.recursive}, whether it is \link[base]{closure} or \link[base]{is.primitive}.
#' Therefore it must be placed before the \link[base]{is.recursive} branching;} 
#' }
#' 
#' ## For all other \link[base]{is.recursive} objects
#' 
#' Function [relaxed_identical] is called ***recursively***, for each \link[base]{$} element of `e1` and `e2`.
#' 
#' ## For \link[base]{S4} objects
#' Function [relaxed_identical] is called ***recursively***, for each \link[base]{@@} \link[methods]{slot} 
#' (which is technically the \link[base]{attributes}) of `e1` and `e2`, 
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
relaxed_identical <- function(e1, e2) {
  
  if ((typeof(e1) %in% c('integer', 'double')) && (typeof(e2) %in% c('integer', 'double'))) {
    return(isTRUE(all.equal.numeric(
      target = e1, current = e2, 
      check.attributes = FALSE
    )))
  }
  
  if (inherits(e1, what = 'formula') && inherits(e2, what = 'formula')) {
    if (identical(e1, e2)) return(TRUE)
    environment(e1) <- environment(e2) <- NULL
    return(identical(e1, e2))
    # return(identical(e1, e2, ignore.environment = TRUE)) # wrong!! formula not \link[base]{closure}
  }

  if (inherits(e1, what = 'function') && inherits(e2, what = 'function')) {
    return(identical(e1, e2, ignore.environment = TRUE))
  }
  
  if (is.recursive(e1) && is.recursive(e2)) {
    if (length(e1) != length(e2)) return(FALSE)
    return(all(mapply(FUN = relaxed_identical, e1, e2, SIMPLIFY = TRUE)))
  } # recursive call; beautiful!!
  
  if (isS4(e1) && isS4(e2)) {
    return(relaxed_identical(e1@.Data, e2@.Data) & relaxed_identical(attributes(e1), attributes(e2)))
  } # recursive call on attributes (not tested, but should be correct)
  
  return(identical(e1, e2)) # exception handling
  
}