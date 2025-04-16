
#' @title Clopper-Pearson Exact Binomial Confidence Interval
#' 
#' @description 
#' Clopper-Pearson exact binomial confidence interval.
#' 
#' @param x positive \link[base]{integer} scalar or \link[base]{vector}, counts
#' 
#' @param n positive \link[base]{integer} scalar or \link[base]{vector}, sample sizes \eqn{n}
#' 
#' @param conf.level,alternative,... additional parameters of function \link[stats]{binom.test}
#' 
#' @returns 
#' Function [binom_confint()] returns a \link[base]{data.frame}.
#' 
#' @examples 
#' binom_confint(0:10, 10L)
#' binom_confint(0:10, 10L, alternative = 'less')
#' binom_confint(0:10, 10L, alternative = 'greater')
#' @keywords internal
#' @importFrom stats binom.test
#' @export
binom_confint <- function(x, n, conf.level = .95, alternative = c('two.sided', 'less', 'greater'), ...) {
  
  if (!is.integer(x) || !length(x) || anyNA(x)) stop('x must be integer')
  if (!is.integer(n) || !length(n) || anyNA(n) || any(n <= 0L)) stop('n must be positive integer')
  if (!is.double(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('illegal level')
  
  alternative <- match.arg(alternative)
  
  ht <- mapply(
    FUN = binom.test, 
    x = x, n = n, # this is not compute intensive; no need to fully vectorize
    MoreArgs = list(
      conf.level = conf.level,
      alternative = alternative
    ), SIMPLIFY = FALSE) 
  
  cint <- ht |> 
    lapply(FUN = `[[`, 'conf.int') |>
    do.call(what = rbind)
  
  x <- ht |>
    vapply(FUN = `[[`, 'statistic', FUN.VALUE = NA_real_) |>
    as.integer()
  
  n <- ht |> 
    vapply(FUN = `[[`, 'parameter', FUN.VALUE = NA_real_) |>
    as.integer()
  
  phat <- ht |>
    vapply(FUN = `[[`, 'estimate', FUN.VALUE = NA_real_)
    
  ret <- sprintf(fmt = '%.1f%% (%.1f%%, %.1f%%)', 1e2*phat, 1e2*cint[,1L], 1e2*cint[,2L])
  nm <- sprintf(fmt = 'Percentage (%.f%% %s-Sided Exact CI)', 1e2*conf.level, switch(alternative, two.sided = '2', '1'))
  
  #ret <- data.frame(
  #  p = ret, 
  #  row.names = sprintf(fmt = '%d / %d', x, n)
  #)
  # `data.frame` does **not** allow duplicated `row.names` !!!
  #names(ret) <- nm0
  
  dim(ret) <- c(length(ret), 1L)
  dimnames(ret) <- list(sprintf(fmt = '%d / %d', x, n), nm)
  
  return(noquote(ret, right = TRUE))
  
}






#' @title View Binomial Confidence Interval
#' 
#' @description ..
#' 
#' @param x a \link[base]{logical} \link[base]{matrix}
#'
#' @examples 
#' swiss |> is.na() |> viewBinomCI()
#' airquality |> is.na() |> viewBinomCI()
#' (airquality$Ozone) |> is.na() |> table() # do simple way
#' 
#' @keywords internal
#' @export
viewBinomCI <- function(x) {
  
  obj <- x; x <- NULL
  if (!is.matrix(obj) || !is.logical(obj)) stop('input must be `logical` `matrix`')
  
  x <- obj |> colSums() |> as.integer()
  id <- (x > 0L)
  if (!any(id)) return(invisible())
  
  n <- obj |> nrow()
  nm <- obj |> colnames()
  
  x_ <- x[id]
  o <- order(x_, decreasing = TRUE)
  
  cbind(
    Variable = nm[id][o], 
    binom_confint(
      x = x_[o], 
      n = n
    )
  ) |> 
    noquote(right = TRUE)
  
}




