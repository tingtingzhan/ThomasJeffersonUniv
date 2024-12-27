
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
#' \item{`attr(.,'group1')`}{\link[stats]{formula}, the highest grouping}
#' }
#' 
#' @examples
#' data(farms, package = 'MASS')
#' interaction(farms[c('Mois', 'Manag')])
#' 
#' (f = nested_(~ Mois/Manag, data = farms))
#' stopifnot(identical(f, nested_(quote(~ Mois/Manag), data = farms)))
#' stopifnot(identical(f, nested_(quote(Mois/Manag), data = farms)))
#' @keywords internal
#' @export
nested_ <- function(lang, data, sep = '.', lex.order = TRUE) {
  
  if (!is.language(lang)) stop('`lang` must be language')
  x <- all.vars(lang)
  
  ret <- interaction(data[x], drop = TRUE, sep = sep, lex.order = lex.order)
  
  id <- if (lex.order) 1L else length(x)
  attr(ret, which = 'group1') <- call(name = '~', as.symbol(x[id]))
  # use `data[[group1[[2L]]]]` to grab the 'factor'
  
  class(ret) <- c('nested', class(ret))
  return(ret)
  
}

