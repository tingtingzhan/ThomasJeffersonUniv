
#' @title Syntactic Sugar to Set (\link[base]{ordered}) \link[base]{factor} \link[base]{levels}
#' 
#' @description
#' Syntactic sugar to set (\link[base]{ordered}) \link[base]{factor} \link[base]{levels}.
#' 
#' @param x \link[base]{integer} \link[base]{vector},
#' dummy coding indices
#' 
#' @param plus \link[base]{integer} scalar, 
#' value to be added to `x`, default `0L`. 
#' If the dummy coding starts from 0 (instead of from 1), then use `plus = 1L`
#' 
#' @param ordered \link[base]{logical} scalar, should the returned \link[base]{factor} be 
#' \link[base]{ordered} (`TRUE`) or unordered (`FALSE`, default)
#' 
#' @param value \link[base]{character} \link[base]{vector},
#' \link[base]{levels} of output \link[base]{factor}
#' 
#' @returns 
#' Syntactic sugar [factor<-] returns a \link[base]{factor}.
#' 
#' Syntactic sugar [ordered<-] returns an \link[base]{ordered} \link[base]{factor}.
#' 
#' @examples
#' (x1 = x2 = sample.int(n = 5L, size = 20, replace = TRUE))
#' factor(x1) = letters[1:5]; x1
#' ordered(x2) = LETTERS[1:5]; x2
#' 
#' set.seed(141); (x10 = x20 = sample.int(n = 5L, size = 20, replace = TRUE) - 1L)
#' factor(x10, plus = 1L) = letters[1:5]; x10
#' ordered(x20, plus = 1L) = LETTERS[1:5]; x20
#' @name factor-set
#' @export
`factor<-` <- function(x, plus = 0L, ordered = FALSE, value) {
  
  if (!is.integer(plus) || length(plus) != 1L || is.na(plus)) stop('`plus` must be an integer scalar')
  x <- x + plus
  
  if (!is.integer(x)) {
    if (is.double(x)) {
      if (all(ceiling(x) == x, na.rm = TRUE)) {
        x <- as.integer(x)
      } else stop('`x` must be convertible to integer')
    }
  } # else do nothing
  
  if (!is.character(value) || anyNA(value)) stop('levels must be character, non-missing')
  
  if (any(x < 1L, x > length(value), na.rm = TRUE)) stop('`x+plus` must be between 1 and ', length(value))
  
  # faster than ?base::structure
  attr(x, which = 'levels') <- value
  class(x) <- c(if (ordered) 'ordered', 'factor')
  return(x)
    
}

#' @rdname factor-set
#' @export
`ordered<-` <- function(x, plus = 0L, value) {
  `factor<-`(x, plus = plus, ordered = TRUE, value = value)
}


