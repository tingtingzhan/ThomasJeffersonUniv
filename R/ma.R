
#' @title Moving Average
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{numeric} \link[base]{vector}
#' 
#' @param order \link[base]{integer} scalar
#' 
#' @references 
#' \url{https://stackoverflow.com/questions/743812/calculating-moving-average}
#' 
#' @note
#' Function [ma] is a simplified version of function \link[forecast]{ma} in package \CRANpkg{forecast}.
#' 
#' Function [ma] is much faster than function `zoo::rollmean()`.
#' 
#' Function [ma] imports function `stats::filter()`,
#' not function `dplyr::filter()`.
#' 
#' @returns
#' Function [ma] returns a time-series \link[stats]{ts} object from workhorse function \link[stats]{filter}.
#' 
#' @examples
#' unclass(ma(1:20, order = 3L))
#' unclass(ma(1:2, order = 3L))
#' @importFrom stats filter
#' @export 
ma <- function(x, order = 5L) {
  nx <- length(x)
  if (nx < order) {
    # ?stats::filter error
    ret <- rep(NA, times = nx)
    storage.mode(ret) <- storage.mode(x)
    return(ret)
  } 
  filter(x = x, filter = rep(1/order, times = order), method = 'convolution', sides = 2)
}

