

#' @title Row and Column Operations Recognizing Full Row/Column Missingness
#' 
#' @description
#' 
#' To perform row and column operations using `na.rm = TRUE`, 
#' and only to return a missing value if a full row/column of the input is missing.
#' 
#' @param x \link[base]{array} of two or more dimensions, or a \link[base]{numeric} \link[base]{data.frame} 
#' 
#' @details
#' 
#' The function [rowSums_()] performs ..
#' 
#' The function [rowMeans_()] performs ..
#' 
#' @returns
#' 
#' The functions [rowSums_()], [rowMeans_()] return a \link[base]{numeric} \link[base]{vector}
#' 
#' @seealso 
#' \link[base]{rowSums} \link[base]{rowMeans}
#' 
#' @note 
#' Potential name clash with \link[matrixStats]{rowMeans2}
#' 
#' @examples 
#' (x = matrix(c(1, 2, NA, 3, NA, NA), byrow = TRUE, ncol = 2L))
#' rowSums(x)
#' rowSums(x, na.rm = TRUE)
#' rowSums_(x)
#' 
#' @name row_fullNA
#' @export
rowSums_ <- function(x) {
  xok <- !is.na(x)
  idx <- (rowSums(xok) == 0L) # all NA rows
  ret <- rowSums(x, na.rm = TRUE)
  ret[idx] <- NA
  return(ret)
}



#' @rdname row_fullNA
#' @export
rowMeans_ <- function(x) {
  xok <- !is.na(x)
  idx <- (rowSums(xok) == 0L) # all NA rows
  ret <- rowMeans(x, na.rm = TRUE)
  ret[idx] <- NA
  return(ret)
}


#' @rdname row_fullNA
#' @importFrom matrixStats rowAnys
#' @export
rowAnys_ <- function(x) {
  if (is.list(x)) x <- do.call(cbind, args = x)
  if (is.data.frame(x)) x <- as.matrix.data.frame(x)
  xok <- !is.na(x)
  idx <- (rowSums(xok) == 0L) # all NA rows
  ret <- rowAnys(x, na.rm = TRUE)
  ret[idx] <- NA
  return(ret)
}
