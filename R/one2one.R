
#' @title One-to-One
#' 
#' @description Existence of one-to-one correspondence
#' 
#' @param x,y ..
#' 
#' @export
one2one <- function(x, y) {
  
  xok <- !is.na(x)
  yok <- !is.na(y)
  if (any(xok != yok)) return(FALSE)
  
  x0 <- x[xok]
  y0 <- y[yok]
  xu <- unique.default(x0) # 'factor' or 'character'
  yu <- unique.default(y0) # 'factor' or 'character'
  if (length(xu) != length(yu)) return(FALSE)
  
  idx <- match(x0, table = xu) # wont have `nomatch`
  idy <- match(y0, table = yu)
  all(idx == idy)
  
}


