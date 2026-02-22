
#' @title Cross-Tabulation by a \link[base]{list} of \link[base]{language}s
#' 
#' @param lang a \link[base]{list} of \link[base]{language} objects
#' 
#' @param envir an \link[base]{environment} or a \link[base]{data.frame}
#' 
#' @param ... additional parameters of the function \link[base]{table}
#' 
#' @seealso 
#' \link[stats]{xtabs}
#' 
#' @examples
#' m1 = quote((bill_dep > 18) & (bill_len > 45))
#' m2 = quote((year == 2009) | sex == 'male')
#' list(m1, m2) |> 
#'  lang_tabs(envir = datasets::penguins, useNA = 'ifany')
#' with(datasets::penguins, expr = lang_tabs(list(m1, m2), useNA = 'ifany'))
#' @keywords internal
#' @export
lang_tabs <- function(lang, envir = parent.frame(), ...) {
  if (!is.list(lang) || !all(vapply(lang, FUN = is.language, FUN.VALUE = NA))) stop('`lang` must be language-list')
  nm <- substitute(lang)[-1L] |> 
    as.list() |>
    lapply(FUN = \(i) {
      if (is.symbol(i)) return(i)
      if (i[[1L]] == 'quote') return(i[[-1L]])
      stop('not supported')
    }) |>
    vapply(FUN = deparse1, FUN.VALUE = '')
  z <- lang |> 
    lapply(FUN = eval, envir = envir) |>
    table(...)
  # do not want to use my [do_plus()] then `xtabs()`
  names(dimnames(z)) <- nm
  return(z)
}