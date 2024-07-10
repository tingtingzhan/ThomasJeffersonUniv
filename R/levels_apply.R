

#' @title Apply a Function to \link[base]{levels} of a \link[base]{factor}
#' 
#' @description
#' Apply a \link[base]{function} to the \link[base]{levels} of a \link[base]{factor}.
#' 
#' @param x \link[base]{factor} object
#' 
#' @param FUN \link[base]{function}
#' 
#' @param ... potential arguments of \link[base]{function} `FUN`
#' 
#' @returns 
#' Function [levels_apply] returns 
#' \itemize{
#' \item{a \link[base]{factor}, if `FUN` returns a \link[base]{character} \link[base]{vector};}
#' \item{a \link[base]{vector}, if otherwise.}
#' }
#' 
#' @examples 
#' (x1 = factor(rep(c('abE', 'fsSG'), times = c(2L, 3L))))
#' tolower(x1)
#' levels_apply(x1, FUN = tolower)
#' 
#' #library(microbenchmark)
#' #x1b = factor(rep(c('abE', 'fsSG'), times = c(1e3L, 1e4L)))
#' #microbenchmark(tolower(x1b), levels_apply(x1b, FUN = tolower))
#' @export
levels_apply <- function(x, FUN, ...) {
  xl <- attr(x, which = 'levels', exact = TRUE)
  yl <- FUN(xl, ...)
  if (length(yl) != length(xl)) stop('level operation must maintain the same length')
  if (!is.character(yl)) return(yl[x]) # 'list', 'numeric', 'logical', etc
  attr(x, which = 'levels') <- yl
  return(factor(x)) # to remove duplicated and NA_character_ levels
  
  # return(.droplevels(x, levels = yl, freq0.rm = FALSE))
  # dont want to use my [.droplevels]; want to make this function vanilla
}



