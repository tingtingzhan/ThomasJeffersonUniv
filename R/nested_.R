
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
#' library(nlme)
#' data(Alfalfa, package = 'nlme')
#' head(Alfalfa)
#' interaction(Alfalfa[c('Block', 'Variety')])
#' 
#' (f = nested_(~ Block/Variety, data = Alfalfa))
#' stopifnot(identical(f, nested_(quote(~ Block/Variety), data = Alfalfa)))
#' stopifnot(identical(f, nested_(quote(Block/Variety), data = Alfalfa)))
#' @keywords internal
#' @export
nested_ <- function(lang, data, sep = '.', lex.order = TRUE) {
  
  if (!is.language(lang)) stop('`lang` must be language')
  x <- all.vars(lang)
  
  ret <- interaction(data[x], drop = TRUE, sep = sep, lex.order = lex.order)
  
  #id <- if (lex.order) 1L else length(x)
  #attr(ret, which = 'group1') <- call(name = '~', as.symbol(x[id]))
  # \pkg{spatstat.grouped} no longer need!
  # I have designed [groupedHyperframe] to handle this in a better way!
  
  #class(ret) <- c('nested', class(ret)) # no longer need. I am not defining any S3 dispatches on this return
  return(ret)
  
}

