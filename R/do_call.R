

#' @title Create a \link[base]{call} of Plus
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @returns 
#' Function [do_plus()] returns a \link[base]{call}
#' 
#' @examples
#' do_plus(letters)
#' 
#' @keywords internal
#' @name Reduce_call
#' @export
do_plus <- function(x) {
  if (!is.character(x) || !length(x) || anyNA(x)) stop('input must be non-missing character vector')
  x <- unique.default(x)
  if (!identical(make.names(x), x)) stop('input must be syntactically valid names')
  
  x |>
    paste(collapse = '+') |> 
    str2lang()
}

#' @rdname Reduce_call
#' @examples
#' do_average(letters)
#' @export
do_average <- function(x) {
  call(name = '/', do_plus(x), length(x))
}


