

#' @title Exclusive-OR Elements
#' 
#' @description Exclusive-OR elements in two vectors.
#' 
#' @param e1,e2 two R objects of the same \link[base]{typeof}
#' 
#' @param name1,name2 (optional) \link[base]{character} scalars, 
#' human-friendly names of `e1` and `e2`.
#' Default is the function call of `e1` and `e2`.
#' 
#' @details 
#' Function [set_xor()] returns the exclusive-OR elements in each of the sets, which is slow
#' and only intended for end-user.
#' 
#' @returns 
#' Function [set_xor()] returns either a \link[base]{list} or a \link[base]{vector}.
#' 
#' @examples 
#' set_xor(1:5, 3:7)
#' set_xor(1:5, 1:3)
#' @seealso \link[base]{setequal}
#' @export
set_xor <- function(e1, e2, name1 = deparse1(substitute(e1)), name2 = deparse1(substitute(e2))) {
  force(name1)
  force(name2)
  if (anyDuplicated(e1)) warning('Duplicate(s) detected in ', name1)
  if (anyDuplicated(e2)) warning('Duplicate(s) detected in ', name2)
  e1 <- unique.default(e1) # could be 'factor'
  e2 <- unique.default(e2)
  
  # ?base::setequal
  only1 <- any(id1 <- is.na(match(e1, table = e2, nomatch = NA_integer_)))
  only2 <- any(id2 <- is.na(match(e2, table = e1, nomatch = NA_integer_)))
  
  if (!only1 && !only2) {
    message('Two sets have same elements.')
    return(invisible())
  }
  
  if (only1 && only2) {
    ret <- list(sort.int(e1[id1]), sort.int(e2[id2]))
    names(ret) <- paste('Only in', sQuote(c(name1, name2)))
    return(ret)
  } 
  
  if (only1) {
    ret <- list(sort.int(e1[id1]))
    names(ret) <- paste('Only in', sQuote(c(name1)))
    return(ret)
  } 
  
  if (only2) {
    ret <- list(sort.int(e2[id2]))
    names(ret) <- paste('Only in', sQuote(c(name2)))
    return(ret)
  }
  
  stop('wont come here')
  
}


