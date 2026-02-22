
#' @title Create Formula from Variable Names using only \link[base]{+}
#' 
#' @description ..
#' 
#' @param lhs,rhs \link[base]{character} \link[base]{vector}s, 
#' variable names appearing in 
#' the left- and right-hand-side of a \link[stats]{formula}
#' 
#' @details 
#' 
#' Only for end-user!!!!
#' 
#' The function `%~%` appends two \link[base]{character} \link[base]{vector}s 
#' of variable names into a \link[stats]{formula}.
#' This is much slower than \link[base]{~} operator, thus should only be used by end-user
#' 
#' @examples
#' character() %~% c('age', 'sex') 
#' '.' %~% c('age', 'sex')
#' 
#' .mapply(`%~%`, dots = list(rhs = c('age', 'sex')), MoreArgs = list(lhs = 'edp'))
#' 
#' @keywords internal
#' @export
`%~%` <- function(lhs, rhs) {
  rhs <- do_plus(rhs)
  cl <- if (!length(lhs)) {
    call(name = '~', rhs)
  } else call(name = '~', do_plus(lhs), rhs)
  cl |>
    eval(envir = .GlobalEnv)
}





