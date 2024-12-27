
##############################################
##############################################
##############################################
##
## Edit in package \pkg{ThomasJeffersonUniv}
##
##############################################
##############################################
##############################################

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
#' \describe{
#' \item{`attr(.,'name1')`}{\link[base]{character} scalar}
#' \item{`attr(.,'f1')`}{\link[base]{factor}}
#' }
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
  
  id <- if (lex.order) 1L else n
  attr(ret, which = 'name1') <- x[id] # old name '1st'
  # attr(ret, which = '1st.levels') <- unique.default(vapply(tmp, FUN = `[[`, id, FUN.VALUE = ''))
  attr(ret, which = 'f1') <- factor(data[[x[id]]])
  
  # attr(ret, which = 'sep') <- sep # no longer needed
  
  class(ret) <- c('nested', class(ret))
  return(ret)
}




#' @export
print.nested <- function(x, ...) {
  x0 <- x
  attributes(x0)[c('name1', 'f1')] <- NULL # , 'sep'
  print.factor(x0, ...)
  lev <- levels(attr(x, which = 'f1', exact = TRUE))
  n0 <- length(lev)
  prt <- if (n0 > 6L) paste(c(lev[1:6], '...'), collapse = ' ') else paste(lev, collapse = ' ')
  message(sprintf(fmt = '%d Independent (%s) Levels: %s', n0, attr(x, which = 'name1'), prt))
}





