
#' @title Create Formula from Variable Names using only '+'
#' 
#' @description ..
#' 
#' @param lhs,rhs \link[base]{character} \link[base]{vector}s, variable names appearing in 
#' the left- and right-hand-side of a \link[stats]{formula}
#' 
#' @details 
#' 
#' Only for end-user!!!!
#' 
#' Function `%~%` appends two \link[base]{character} \link[base]{vector}s 
#' of variable names into a \link[stats]{formula}.
#' This is much slower than \link[base]{~} operator, thus should only be used by end-user
#' 
#' @examples
#' xs = c('age', 'sex')
#' character() %~% xs 
#' '.' %~% xs
#' 
#' .mapply(`%~%`, dots = list(rhs = c('age', 'sex')), MoreArgs = list(lhs = 'edp'))
#' 
#' @keywords internal
#' @export
`%~%` <- function(lhs, rhs) {
  rhs <- do_plus(rhs)
  if (!length(lhs)) return(eval(call(name = '~', rhs), envir = .GlobalEnv))
  return(eval(call(name = '~', do_plus(lhs), rhs), envir = .GlobalEnv))
}


if (FALSE) {
  library(microbenchmark)
  e1 = 'a'; e2 = 'b'; microbenchmark(
    call('~', as.symbol(e1), as.symbol(e2)), # better
    str2lang(paste(e1, e2, sep = '~'))
  )
}



