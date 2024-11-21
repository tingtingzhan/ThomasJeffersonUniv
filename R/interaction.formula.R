
#' @title A \link[stats]{formula} Interface for \link[base]{interaction}
#' 
#' @description
#' A \link[stats]{formula} interface for function \link[base]{interaction}.
#' 
#' @param formula \link[stats]{formula}
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param ... parameters `drop`, `sep` and `lex.order` of function \link[base]{interaction}
#' 
#' @details
#' Function [interaction.formula] is useful when the input \link[base]{factor}s are
#' specified as a \link[stats]{formula} based on a \link[base]{data.frame}, e.g., in function \link[base]{split.data.frame}.
#' 
#' 
#' 
#' @note
#' R base function \link[base]{interaction} correctly handles syntactically invalid names (see \link[base]{make.names}).
#' 
#' @examples
#' data(farms, package = 'MASS')
#' stopifnot(identical(with(farms, interaction(Mois, Manag)), 
#'   interaction.formula(~ Mois + Manag, data = farms)))
#' stopifnot(identical(with(farms, interaction(Mois, Manag, drop = TRUE)),
#'   interaction.formula(~ Mois + Manag, data = farms, drop = TRUE)))
#'   
#' # syntactically invalid names, okay!
#' (x = data.frame('a b' = 1:3, 'c d' = LETTERS[1:3], check.names = FALSE))
#' (ret = interaction.formula(~ `a b` + `c d`, data = x, drop = TRUE))
#' stopifnot(identical(ret, with(x, interaction(`a b`, `c d`, drop = TRUE))))
#' @export
interaction.formula <- function(formula, data, ...) {
  if (!is.call(formula) || formula[[1L]] != '~' || length(formula) != 2L) stop('`formula` must be 2-sided')
  xs <- lapply(all.vars(formula), FUN = as.symbol)
  with(data, expr = do.call(what = interaction, args = c(xs, list(...))))
}

