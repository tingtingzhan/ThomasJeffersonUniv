

#' @title Create a \link[base]{call} of Plus
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @returns 
#' Function [do_plus] returns a \link[base]{call}
#' 
#' @examples
#' do_plus(letters[1:3])
#' 
#' @keywords internal
#' @name Reduce_call
#' @export
do_plus <- function(x) {
  if (!is.character(x) || !length(x) || anyNA(x)) stop('input must be non-missing character vector')
  if (!identical(make.names(x), x)) stop('input must be syntactically valid names')
  
  #.plus_ <- function(e1, e2) call(name = '+', e1, e2)
  # Reduce(f = .plus_, x = lapply(x, FUN = as.symbol)) # much slower!!!
  str2lang(s = paste(x, collapse = '+'))
}

#' @rdname Reduce_call
#' @examples
#' do_average(letters[1:3])
#' @export
do_average <- function(x) {
  call(name = '/', do_plus(x), length(x))
}


