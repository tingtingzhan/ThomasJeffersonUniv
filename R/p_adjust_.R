
#' @title Perform All \eqn{p}-value Adjustments
#' 
#' @description 
#' Perform all \eqn{p}-value adjustments.
#' 
#' @param p \link[base]{numeric} \link[base]{vector}
#' 
#' @details 
#' Function [p_adjust_] speeds up function \link[stats]{p.adjust}. 
#' 
#' @returns
#' Function [p_adjust_] returns a \link[base]{matrix} of adjusted p-values using all available \link[stats]{p.adjust.methods}.
#' 
#' @examples
#' \donttest{for (i in seq_len(10L)) {
#'  n = sample(20:30, size = 1L)
#'  p = runif(n, min = .Machine$double.eps, max = 1 - .Machine$double.eps)
#'  (tmp = p_adjust_(p))
#'  stopifnot(identical(tmp, cbind(
#'   holm = p.adjust(p, 'holm'),
#'   hochberg = p.adjust(p, 'hochberg'),
#'   hommel = p.adjust(p, 'hommel'),
#'   bonferroni = p.adjust(p, 'bonferroni'),
#'   BH = p.adjust(p, 'BH'),
#'   BY = p.adjust(p, 'BY'),
#'   none = p)))
#' }}
#' @importFrom stats p.adjust.methods
#' @export
p_adjust_ <- function(p) {
  
  if (!is.double(p) || any(p < 0, p > 1, na.rm = TRUE)) stop('p must be double probabilities')
  pok <- !is.na(p)
  p0 <- c(p[pok]) # force vector
  n <- sum(pok)
  if (n <= 1L) stop('no pvalue adjustment for len-1 or len-0 input')
  
  method <- setdiff(p.adjust.methods, y = c('fdr', if (n == 2L) 'hommel'))
  # 'fdr' is 'BH'
  # 'hommel' for n == 2L is 'hochberg'
  
  sq <- seq_len(n) # sequence
  rsq <- rev.default(sq) # reversed sequence
  o <- order(p0)
  p1 <- p0[o] # sorted `p0` in increasing order
  oo <- order(o)
  od <- order(p0, decreasing = TRUE)
  p1d <- p0[od] # sorted `p0` in decreasing order
  ood <- order(od)
  
  ret <- array(p, dim = c(length(p), length(method)), dimnames = list(names(p), method))
  
  ret[pok, 'bonferroni'] <- n*p0
  
  ret[pok, 'holm'] <- cummax(rsq * p1)[oo]
  
  if (any(method == 'hommel')) {
    q <- pa <- rep.int(min(n * p1/sq), times = n)
    for (j in (n - 1L):2L) {
      ij <- seq_len(n - j + 1L)
      i2 <- (n - j + 2L):n
      q1 <- min(j * p1[i2]/(2L:j))
      q[ij] <- pmin.int(j * p1[ij], q1)
      q[i2] <- q[n - j + 1L]
      pa <- pmax.int(pa, q)
    }
    ret[pok, 'hommel'] <- pmax.int(pa, p1)[oo]
  }
  
  ret[pok, 'hochberg'] <- cummin(sq * p1d)[ood]
  
  ret[pok, 'BH'] <- cummin(n/rsq * p1d)[ood]
  
  ret[pok, 'BY'] <- cummin(sum(1/sq) * n/rsq * p1d)[ood]
  
  ret[] <- pmin.int(1, ret)
  return(ret)
}












