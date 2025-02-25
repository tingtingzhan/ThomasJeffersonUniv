
#' @title \link[base]{grepl}, but recognize `NA`
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @param ... additional parameters of function \link[base]{grepl}
#' 
#' @examples
#' grepl_(pattern = '^a', x = c('a', 'b', NA_character_))
#' @export
grepl_ <- function(x, ...) {
  ret <- grepl(x, ...)
  ret[is.na(x)] <- NA
  return(ret)
}


#' @title gsub_yes
#' 
#' @param pattern \link[base]{character} scalar
#' 
#' @param replacement \link[base]{character} scalar
#' 
#' @param x \link[base]{character} \link[base]{vector}
#' 
#' @param ... additional parameters of function \link[base]{grepl}
#' 
#' @returns
#' Function 
#' 
#' @examples
#' gsub_yes('^a', x = c('a', 'b', NA_character_))
#' @export
gsub_yes <- function(pattern, replacement = 'YES', x, ...) {
  ret <- character(length = length(x))
  id <- grepl_(pattern = pattern, x = x, ...)
  ret[which(id)] <- replacement # positive
  ret[which(!id)] <- '-' # negative
  ret[is.na(id)] <- '.' # missing
  return(ret)
}

