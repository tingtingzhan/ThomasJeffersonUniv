

#' @title format_named
#' 
#' @param x \link[base]{character} \link[base]{vector}, 
#' or a \link[base]{list} of \link[base]{character} object.
#' Input `x` must be named
#' 
#' @param sep \link[base]{character} scalar, see \link[base]{paste}
#' 
#' @returns
#' Function [format_named()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' x1 = c(a = 'a1', bc = '2\n3')
#' x1 |> format_named() |> cat(sep = '\n')
#' noout = lapply(format_named(x1), FUN = message)
#' 
#' x2 = list(a = '1\n2', b = character(), cd = '3\n4', efg = '5\n6\n7')
#' x2 |> format_named() |> cat(sep = '\n')
#' noout = lapply(format_named(x2), FUN = message)
#' 
#' x3 = c(a = '1\n2')
#' cat(format_named(x3), sep = '\n')
#' noout = lapply(format_named(x3), FUN = message)
#' 
#' @keywords internal
#' @export
format_named <- function(x, sep = ': ') {
  
  x0 <- x |>
    vapply(FUN = paste, collapse = ' ', FUN.VALUE = '') |> 
    trimws()
  x1 <- x0[nzchar(x0)]
  if (!length(nm <- names(x1))) stop('input must be named')
  if (!all(nzchar(nm))) stop('do not allow empty name!')
  
  x2 <- strsplit(x1, split = '\n')
  nx <- lengths(x2)
  if (!all(nx == 1L)) { # some element(s) contains '\n'
    x1 <- unlist(x2, use.names = FALSE)
    xnm. <- character(length = length(x1))
    xnm.[cumsum(nx)] <- nm # has zchar in `xnm.`
  } else xnm. <- nm
  
  xnm <- format.default(xnm., justify = 'right')
  
  ret <- paste(xnm, x1, sep = sep) |> style_bold()
  id_green <- if (length(nx) == 1L) {
    rep(TRUE, times = nx) # all green
  } else {
    mapply(FUN = rep, c(TRUE, FALSE), times = nx, SIMPLIFY = FALSE) |>
      unlist(use.names = FALSE) |>
      suppressWarnings()
  }
  ret[id_green] <- ret[id_green] |> col_green()
  ret[!id_green] <- ret[!id_green] |> col_cyan()
  return(ret)
  
}

# https://gist.github.com/upsilun/4a85ab3bc7cf92e8acde720c6eb7ddea

