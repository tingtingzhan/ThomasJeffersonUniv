
##############################################
##############################################
##############################################
##
## Edit in package ThomasJeffersonUniv
##
##############################################
##############################################
##############################################




#' @title A \link[base]{language} Interface for \link[base]{interaction}
#' 
#' @description
#' A \link[base]{language} interface for function \link[base]{interaction}.
#' 
#' @param lang any \link[base]{language}, e.g., \link[stats]{formula}, \link[base]{call} or \link[base]{symbol}
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param ... parameters `drop`, `sep` and `lex.order` of function \link[base]{interaction}
#' 
#' @details
#' Function [interaction_lang] is useful when the input \link[base]{factor}s are
#' specified as a \link[base]{language} based on a \link[base]{data.frame}, e.g., in function \link[base]{split.data.frame}.
#' 
#' 
#' 
#' @note
#' R base function \link[base]{interaction} correctly handles syntactically invalid names (see \link[base]{make.names}).
#' 
#' @examples
#' data(farms, package = 'MASS')
#' (f1 = interaction_lang(~ Mois + Manag, data = farms))
#' stopifnot(identical(with(farms, interaction(Mois, Manag)), f1))
#' (f2 = interaction_lang(~ Mois + Manag, data = farms, drop = TRUE))
#' stopifnot(identical(with(farms, interaction(Mois, Manag, drop = TRUE)), f2))
#'   
#' # first argument can be 'call' with '~'
#' stopifnot(identical(f2,
#'  interaction_lang(call('~', call('+', quote(Mois), quote(Manag))), data = farms, drop = TRUE)))
#'  
#' # or can be just any 'language'
#' stopifnot(identical(f2,
#'  interaction_lang(call('+', quote(Mois), quote(Manag)), data = farms, drop = TRUE)))
#'  
#' # syntactically invalid names, okay!
#' (x = data.frame('a b' = 1:3, 'c d' = LETTERS[1:3], check.names = FALSE))
#' (ret = interaction_lang(~ `a b` + `c d`, data = x, drop = TRUE))
#' stopifnot(identical(ret, with(x, interaction(`a b`, `c d`, drop = TRUE))))
#' @export
interaction_lang <- function(lang, data, ...) {
  if (!is.language(lang)) stop('`lang` must be language')
  xs <- lapply(all.vars(lang), FUN = as.symbol)
  with(data, expr = do.call(what = interaction, args = c(xs, list(...))))
}

