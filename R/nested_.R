
##############################################
##############################################
##############################################
##
## Edit in package \pkg{ThomasJeffersonUniv}
##
##############################################
##############################################
##############################################

interaction_lang <- function() .Defunct(new = 'nested_')


#' @title Nested \link[base]{factor}s
#' 
#' @description
#' ..
#' 
#' @param lang any \link[base]{language} of a nested structure, e.g., `~c1/c2` or `c1/c2`
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param drop see function \link[base]{interaction}, default `TRUE`
#' 
#' @param sep see function \link[base]{interaction}
#' 
#' @param lex.order see function \link[base]{interaction}, default `TRUE`
#' 
#' @details
#' Function [nested_] ..
#' 
#' @note
#' R base function \link[base]{interaction} correctly handles syntactically invalid names (see \link[base]{make.names}).
#' 
#' @returns
#' Function [nested_] returns a \link[base]{factor} (from function \link[base]{interaction}) with additional \link[base]{attributes},
#' 
#' @examples
#' data(farms, package = 'MASS')
#' interaction(farms[c('Mois', 'Manag')], drop = TRUE, lex.order = TRUE)
#' (f1 = nested_(~ Mois/Manag, data = farms, drop = TRUE, lex.order = TRUE))
#'   
#' # first argument can be 'call' starting with '~'
#' stopifnot(identical(f1, nested_(quote(~ Mois/Manag), data = farms, drop = TRUE)))
#'  
#' # or can be just any 'language'
#' stopifnot(identical(f1, nested_(quote(Mois/Manag), data = farms, drop = TRUE)))
#'  
#' @keywords internal
#' @export
nested_ <- function(lang, data, drop = TRUE, sep = '.', lex.order = TRUE) {
  if (!is.language(lang)) stop('`lang` must be language')
  x <- all.vars(lang)
  n <- length(x)
  xval <- data[x]
  ret <- interaction(xval, drop = drop, sep = sep, lex.order = lex.order)
  
  # ?base::interaction does not return processed levels..
  lev <- attr(ret, which = 'levels', exact = TRUE)
  tmp <- strsplit(lev, split = sep, fixed = TRUE)
  if (any(n != lengths(tmp, use.names = FALSE))) stop('original levels contains `sep`?')
  attr(ret, which = 'level0') <- unique.default(vapply(tmp, FUN = `[[`, if (lex.order) 1L else n, FUN.VALUE = ''))
  
  class(ret) <- c('nested', class(ret))
  return(ret)
}




#' @export
print.nested <- function(x, ...) {
  x0 <- x
  attr(x0, which = 'level0') <- NULL
  print.factor(x0, ...)
  level0 <- attr(x, which = 'level0', exact = TRUE)
  message(sprintf(fmt = '%d Independent Levels: %s', length(level0), paste(level0, collapse = ' ')))
}





