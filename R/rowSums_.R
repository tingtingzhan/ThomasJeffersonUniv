

#' @title Row Operations Recognizing Full Row Missingness
#' 
#' @description
#' 
#' To perform row operations using `na.rm = TRUE`, 
#' and only to return a missing value if a full row of the input is missing.
#' 
#' @param x \link[base]{array} of two or more dimensions, or a \link[base]{numeric} \link[base]{data.frame} 
#' 
#' @details
#' 
#' The function [rowSums_()] performs ..
#' 
#' The function [rowMeans_()] performs ..
#' 
#' The function [rowAnys_()] performs ..
#' 
#' @returns
#' 
#' The functions [rowSums_()], [rowMeans_()] return 
#' a \link[base]{numeric} \link[base]{vector}.
#' 
#' The function [rowAnys_()] return 
#' a \link[base]{logical} \link[base]{vector}.
#' 
#' @seealso 
#' \link[base]{rowSums}; 
#' \link[base]{rowMeans};
#' \link[matrixStats]{rowMeans2} (potential name clash)
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
  ret <- rowSums(x, na.rm = TRUE)
  ret[rowSums(!is.na(x)) == 0L] <- NA_real_ # all NA rows
  return(ret)
}



#' @rdname row_fullNA
#' @export
rowMeans_ <- function(x) {
  ret <- rowMeans(x, na.rm = TRUE)
  ret[rowSums(!is.na(x)) == 0L] <- NA_real_ # all NA rows
  return(ret)
}


#' @rdname row_fullNA
#' @importFrom matrixStats rowAnys
#' @export
rowAnys_ <- function(x) {
  if (is.list(x)) x <- do.call(cbind, args = x)
  if (is.data.frame(x)) x <- as.matrix.data.frame(x)
  ret <- rowAnys(x, na.rm = TRUE)
  ret[rowSums(!is.na(x)) == 0L] <- NA # all NA rows
  return(ret)
}
