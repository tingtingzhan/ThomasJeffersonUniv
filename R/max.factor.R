
#' @title \link[base]{max} and \link[base]{min} of \link[base]{factor}
#' 
#' @description
#' ..
#' 
#' @param ... one \link[base]{factor} object
#' 
#' @param na.rm \link[base]{logical} scalar
#' 
#' @returns 
#' Functions [max.factor] and [min.factor] both return a \link[base]{factor}.
#' 
#' @examples
#' (x = structure(c(NA_integer_, sample.int(3L, size = 20L, replace = TRUE)),
#'  levels = letters[1:3], class = 'factor'))
#' max(x, na.rm = FALSE)
#' max(x, na.rm = TRUE)
#' min(x, na.rm = TRUE)
#' 
#' (x0 = structure(rep(NA_integer_, times = 20L),
#'  levels = letters[1:3], class = 'factor'))
#' max(x0)
#' min(x0)
#' @name extremes_factor
#' @export max.factor
#' @export
max.factor <- function(..., na.rm = FALSE) {
  
  dots <- list(...)
  if (length(dots) != 1L) stop('accepts one and only one `factors`')
  x <- dots[[1L]]
  
  lv <- attr(x, which = 'levels', exact = TRUE)
  x0 <- unclass(x)
  
  ret <- if (all(is.na(x0))) {
    # no matter `na.rm` or not
    NA_integer_
  } else max(x0, na.rm = na.rm)
  
  return(structure(ret, levels = lv, class = class(x))) # (ordered) factor
  
}


#' @rdname extremes_factor
#' @export min.factor
#' @export
min.factor <- function(..., na.rm = FALSE) {
  
  dots <- list(...)
  if (length(dots) != 1L) stop('accepts one and only one `factors`')
  x <- dots[[1L]]
  
  lv <- attr(x, which = 'levels', exact = TRUE)
  x0 <- unclass(x)
  
  ret <- if (all(is.na(x0))) {
    # no matter `na.rm` or not
    NA_integer_
  } else min(x0, na.rm = na.rm)
  
  return(structure(ret, levels = lv, class = class(x))) # (ordered) factor
  
}