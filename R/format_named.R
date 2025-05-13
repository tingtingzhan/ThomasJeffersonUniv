

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
#' c(a = 'a1', bc = '2\n3') |> format_named()
#' 
#' list(a = '1\n2', b = character(), cd = '3\n4', efg = '5\n6\n7') |> format_named()
#' 
#' c(a = '1\n2') |> format_named()
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
  
  x2 <- x1 |>
    strsplit(split = '\n')
  
  nx <- lengths(x2)
  cx <- nx |> cumsum()
  
  if (!all(nx == 1L)) { # some element(s) contains '\n'
    x1 <- x2 |>
      unlist(use.names = FALSE) # overwrite `x1` !!!
    xnm. <- character(length = length(x1))
    xnm.[cx] <- nm # now has zchar in `xnm.`
  } else xnm. <- nm
  
  xnm <- xnm. |>
    format.default(justify = 'right')
  
  ret <- paste(xnm, x1, sep = sep)
  
  mapply(
    FUN = `:`, 
    c(1L, cx[-length(cx)] + 1L) |> unname(),
    cx |> unname(), 
    SIMPLIFY = FALSE) |> # `real` indices
    lapply(FUN = \(i) {
      ret[i] |>
        lapply(FUN = message)
      cat('\n')
      # make use of RStudio 2025 !!!
    })
  
  return(invisible())
  
}





# https://gist.github.com/upsilun/4a85ab3bc7cf92e8acde720c6eb7ddea

