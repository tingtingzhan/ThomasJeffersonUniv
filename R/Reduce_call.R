

#' @title Create a \link[base]{call} of Plus
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @returns 
#' Function [Reduce_plus] returns a \link[base]{call}
#' 
#' @examples
#' Reduce_plus(letters[1:3])
#' 
#' @keywords internal
#' @name Reduce_call
#' @export
Reduce_plus <- function(x) {
  if (!is.character(x) || !length(x) || anyNA(x)) stop('input must be non-missing character vector')
  if (!identical(make.names(x), x)) stop('input must be syntactically valid names')
  Reduce(f = .plus_, x = lapply(x, FUN = as.symbol))
}

# old name `call_avg`

#' @rdname Reduce_call
#' @examples
#' Reduce_average(letters[1:3])
#' @export
Reduce_average <- function(x) {
  call(name = '/', Reduce_plus(x), length(x))
}


# old name `call_plus`
.plus_ <- function(e1, e2) call(name = '+', e1, e2)






if (FALSE) {
  e1 = quote(a)
  e2 = quote(b)
  library(microbenchmark)
  microbenchmark(
    as.call(list(quote(`+`), e1, e2)), # fast!
    call(name = '+', e1, e2), # fast!
    .plus_(e1, e2) # why much slower??
  )
  
}