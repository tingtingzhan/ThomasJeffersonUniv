
#' @title \link[base]{factor} Return for \link[dplyr]{case_match}
#' 
#' @description
#' ..
#' 
#' @param .x,...,.default,.ptype see \link[dplyr]{case_match}
#' 
#' @details
#' If argument `.default` is missing, function [case_match_factor] converts 
#' the return of \link[dplyr]{case_match} into a \link[base]{factor}.
#' The order of \link[base]{levels} follows the order of formulas in
#' \link[rlang:dyn-dots]{dynamic dots} `...`.
#' 
#' @returns
#' Function [case_match_factor] returns a \link[base]{factor}.
#' 
#' @examples
#' # from ?dplyr::case_match
#' x = c('a', 'b', 'a', 'd', 'b', NA, 'c', 'e')
#' dplyr::case_match(x, 'a' ~ 1, 'b' ~ 2, 'c' ~ 3, 'd' ~ 4)
#' case_match_factor(x, 'a' ~ 1, 'b' ~ 2, 'c' ~ 3, 'd' ~ 4)
#' case_match_factor(x, 'a' ~ 1, 'd' ~ 4, 'b' ~ 2, 'c' ~ 3) # order matters!
#' 
#' y = c(1, 2, 1, 3, 1, NA, 2, 4)
#' case_match_factor(y, NA ~ 0, .default = y)
#' @importFrom dplyr case_match
#' @export
case_match_factor <- function(.x, ..., .default = NULL, .ptype = NULL) {
  
  cl <- match.call()
  cl[[1L]] <- quote(case_match)
  ret0 <- eval(cl, envir = parent.frame())
  
  if (length(cl$.default)) {
    message('Presence of ', sQuote('.default'), ' prohibits conversion to factor')
    return(ret0)
  }
  
  arg_ <- as.list(cl)[-1L]
  target <- arg_[!nzchar(names(arg_))]
  lev_ <- vapply(target, FUN = function(i) as.character(i[[3L]]), FUN.VALUE = '')
  factor(ret0, levels = lev_)
  
}