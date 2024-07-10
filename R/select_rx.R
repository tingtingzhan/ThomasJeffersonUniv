
#' @title Select by Regular Expression
#' 
#' @description ..
#' 
#' @param pattern \link[base]{character} scalar, regular expression
#' 
#' @param envir an \link[base]{environment}, 
#' a \link[base]{list}, 
#' a \link[base]{data.frame}, 
#' or a \link[base]{matrix}.
#' 
#' @param ... additional parameters of \link[base]{grep}, most importantly `invert`
#' 
#' @returns  
#' 
#' Function [select_rx] selects
#' 
#' \itemize{
#' \item {columns from a \link[base]{data.frame} by `pattern` matching against its \link[base]{names},
#' and returns a \link[base]{data.frame}.}
#' \item {elements from a \link[base]{list} by `pattern` matching against its \link[base]{names},
#' and returns a \link[base]{list}.}
#' \item {columns from a \link[base]{matrix} by `pattern` matching against its \link[base]{colnames},
#' and returns a \link[base]{matrix}.}
#' \item {objects in an \link[base]{environment} by `pattern` matching against the names of variables within,
#' and returns a **\link[base]{list}** 
#' instead of an \link[base]{environment}.}
#' }
#'  
#' 
#' @examples
#' head(select_rx('\\.Width$', envir = iris))
#' head(select_rx('\\.Length$', envir = iris, invert = TRUE))
#' 
#' select_rx('^Rural', VADeaths)
#' 
#' with(head(iris), expr = {
#'  select_rx(pattern = '\\.Width$')
#' })
#' @export
select_rx <- function(pattern, envir = parent.frame(), ...) {
  
  if (!is.character(pattern) || length(pattern) != 1L || anyNA(pattern) || !nzchar(pattern)) stop('regular expression in `pattern` must be len-1 character')
  
  if (is.environment(envir)) {
    # most frequently used; via ?base::within and ?base::with
    nms <- ls(envir = envir, all.names = TRUE, pattern = pattern, sorted = FALSE)
    return(mget(x = nms, envir = envir))
  }
  
  if (inherits(envir, what = 'data.table')) stop('data.table not supported, yet')
  
  if (is.matrix(envir)) {
    id <- grep(pattern = pattern, x = dimnames(envir)[[2L]], value = FALSE, ...)
    if (!length(id)) return(invisible())
    return(envir[, id, drop = FALSE])
  }

  if (is.recursive(envir)) {
    # 'list', 'data.frame'
    # 'tibble'
    id <- grep(pattern = pattern, x = names(envir), value = FALSE, ...)
    if (!length(id)) return(invisible())
    return(envir[id])
  }
  
  stop(sQuote(class(envir)), ' `envir` not supported')

}





