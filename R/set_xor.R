

#' @title Exclusive-OR Elements
#' 
#' @description Exclusive-OR elements in two vectors.
#' 
#' @param e1,e2 two R objects of the same \link[base]{typeof}
#' 
#' @param name1,name2 (optional) \link[base]{character} scalars, 
#' human-friendly names of `e1` and `e2`.
#' Default is the function call of `e1` and `e2`.
#' 
#' @details 
#' The function [set_xor()] returns the exclusive-OR elements in each of the sets, which is slow
#' and only intended for end-user.
#' 
#' @returns 
#' The function [set_xor()] returns a \link[stats]{listof} of \link[base]{vector}s.
#' 
#' @seealso 
#' \link[base]{setequal}
#' 
#' @examples 
#' set_xor(1:5, 3:7)
#' set_xor(1:5, 1:3)
#' @keywords internal
#' @export
set_xor <- function(e1, e2, name1 = deparse1(substitute(e1)), name2 = deparse1(substitute(e2))) {
  
  nm <- c(name1, name2) |>
    col_br_magenta() |> style_bold() |>
    paste('Only in', . = _)
  
  if (anyDuplicated(e1)) warning('Duplicate(s) detected in ', name1)
  if (anyDuplicated(e2)) warning('Duplicate(s) detected in ', name2)
  e1 <- e1 |>
    unique.default() # could be 'factor'
  e2 <- e2 |>
    unique.default()
  
  # ?base::setequal
  id1 <- e1 |>
    match(x = _, table = e2, nomatch = NA_integer_) |>
    is.na()
  id2 <- e2 |>
    match(x = _, table = e1, nomatch = NA_integer_) |>
    is.na()
  
  z <- list(e1[id1], e2[id2]) |>
    lapply(FUN = sort.int)
  names(z) <- nm
  
  z <- z[lengths(z) > 0L]
  if (!length(z)) {
    message('Two sets have same elements.')
    return(invisible())
  }
  class(z) <- 'listof'
  return(z)

}


