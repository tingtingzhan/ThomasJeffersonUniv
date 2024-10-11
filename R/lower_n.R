

#' @title The \eqn{n}th Lower Edge(s) of a Matrix
#' 
#' @description 
#' The \eqn{n}th lower edge(s) of a \link[base]{matrix}
#' 
#' @param x \link[base]{matrix}
#' 
#' @param n \link[base]{integer} scalar or \link[base]{vector}, 
#' order(s) of the lower edge(s)
#' 
#' @details 
#' Function [lower_n] extends \link[base]{lower.tri}, so that 
#' the \link[base]{logical} indices of \eqn{n}th lower-edge(s) elements are returned.
#' 
#' @returns 
#' Function [lower_n] returns a \link[base]{logical} \link[base]{matrix}.
#' 
#' @examples 
#' # ?euro.cross
#' dim(euro.cross) # square matrix
#' lower_n(euro.cross) # I love dimnames 
#' lower.tri(euro.cross) # no dimnames
#' lower_n(euro.cross, 1:2)
#' 
#' dim(VADeaths) # non square matrix
#' lower_n(VADeaths, n = 1:2)
#' 
#' (x = array(1, dim = c(1, 1)))
#' lower_n(x) # exception handling
#' @export
lower_n <- function(x, n = seq_len(.dim[1L] - 1L)) {
  .dim <- dim(x)
  if (!length(n)) return(invisible())
  if (!is.integer(n)) stop('n must be integer') 
  if (any(abs(n) >= .dim[1L])) stop('n needs to be from 1 to nrow(x)-1')
  lo <- .row(.dim) - .col(.dim) # ?base::lower.tri
  array(lo %in% n, dim = .dim, dimnames = dimnames(x))
}

