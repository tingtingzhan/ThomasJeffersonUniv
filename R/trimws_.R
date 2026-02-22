
#' @title Remove Leading/Trailing and Duplicated (Symbols that Look Like) White Spaces
#' 
#' @description
#' To remove leading/trailing and duplicated (symbols that look like) white spaces.
#' 
#' @param x an R object with \link[base]{typeof} being \link[base]{character}
#' 
#' @details 
#' The function [trimws_()] is more aggressive than 
#' the function \link[base]{trimws}, 
#' that it removes
#' 
#' \itemize{
#' \item {non-UTF-8 characters}
#' \item {duplicated white spaces}
#' \item {symbols that look like white space, such as `\u00a0` (no-break space)}
#' }
#' 
#' @note 
#' The function \link[base]{gsub} keeps \link[base]{attributes}.
#' 
#' @returns 
#' The function [trimws_()] returns an object of \link[base]{typeof} \link[base]{character}.
#' 
#' @examples 
#' stopifnot(!identical('\u00a0', ' '))
#' (x = c(A = ' a  b  ', b = 'a .  s', ' a  ,  b ; ', '\u00a0  ab '))
#' base::trimws(x)
#' trimws_(x)
#' 
#' (x0 = ' ab  \xa0cd ')
#' tryCatch(base::trimws(x0), error = identity)
#' trimws_(x0)
#' 
#' @keywords internal
#' @export
trimws_ <- function(x) {
  
  # http://stackoverflow.com/questions/14737266/removing-multiple-spaces-and-trailing-spaces-using-gsub
  # https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces
  
  if (typeof(x) != 'character') stop('must be typeof-character input')
  
  # https://stackoverflow.com/questions/17291287/how-to-identify-delete-non-utf-8-characters-in-r
  Encoding(x) <- 'UTF-8'
  
  x |> 
    iconv(from = 'UTF-8', to = 'UTF-8', sub = '') |> ## replace any non UTF-8 by ''
    gsub(pattern = '\u00a0', replacement = '', x = _) |> # symbols that look like a whitespace
    gsub(pattern = '^ *|(?<= ) | *$', replacement = '', x = _, perl = TRUE)
  
}

