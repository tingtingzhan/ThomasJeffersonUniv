
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
#' 
#' # some exceptions
#' x = 1:4
#' factor(x) = c('a', 'b', 'c', 'c')
#' x # duplicated levels dropped
#' 
#' x = 1:4
#' factor(x) = c('a', 'b', NA_character_, 'c')
#' x # missing level converted to missing entry
#' 
#' x = 1:4
#' ordered(x) = c('a', 'b', NA_character_, 'c')
#' x # correctly ordered
#' 
#' (x = array(sample.int(4, size = 20, replace = TRUE), dim = c(4,5)))
#' factor(x) = letters[1:4]
#' x # respects other attributes
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
  
  if (!is.character(value)) stop('levels must be character, non-missing')
  # anyNA(value) # allowed
  
  if (any(x < 1L, x > length(value), na.rm = TRUE)) stop('`x+plus` must be between 1 and ', length(value))
  
  # faster than ?base::structure
  attr(x, which = 'levels') <- value
  class(x) <- c(if (ordered) 'ordered', 'factor')
  
  ret <- factor(x) # drop duplicated levels
  atr <- attributes(x)
  atr$levels <- atr$class <- NULL # levels being processed
  attributes(ret)[names(atr)] <- atr
  return(ret)
    
}

#' @rdname factor-set
#' @export
`ordered<-` <- function(x, plus = 0L, value) {
  `factor<-`(x, plus = plus, ordered = TRUE, value = value)
}


