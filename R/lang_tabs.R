
#' @title Cross-Tabulation by a \link[base]{list} of \link[base]{language}s
#' 
#' @param lang a \link[base]{list} of \link[base]{language} objects
#' 
#' @param data a \link[base]{data.frame}
#' 
#' @param ... additional parameters of function \link[base]{table}
#' 
#' @seealso \link[stats]{xtabs}
#' 
#' @keywords internal
#' @export
lang_tabs <- function(lang, data, ...) {
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
    lapply(FUN = eval, envir = data) |>
    table(...)
  names(dimnames(z)) <- nm
  return(z)
}