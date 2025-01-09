
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
#' Function [ma] is much faster than function \link[zoo]{rollmean} in package \CRANpkg{zoo}.
#' 
#' Function [ma] imports function \link[stats]{filter} from package \pkg{stats},
#' not function \link[dplyr]{filter} from package \CRANpkg{dplyr}.
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

