

#' @title Split \link[base]{character} \link[base]{vector}
#' 
#' @description
#' Split \link[base]{character} \link[base]{vector}, into keywords or by order of appearance.
#' 
#' @param x \link[base]{character} \link[base]{vector}, each element being a set of 
#' keywords separated by a symbol (e.g., `','`)
#' 
#' @param key (optional for function [splitKey]) \link[base]{character} \link[base]{vector}, 
#' user-specified keywords.  Default to all keywords appearing in input `x`
#' 
#' @param nm (for function [splitOrd]) \link[base]{character} \link[base]{vector}
#' 
#' @param data.name (optional) \link[base]{character} scalar or \link[base]{name},
#' name of input `x`
#' 
#' @param envir `NULL` or \link[base]{environment}. 
#' If an \link[base]{environment} is specified, then assign the result to it
#' (i.e., when used inside \link[base]{within.data.frame}).
#' 
#' @param ... potential parameters of \link[base]{strsplit}, most importantly `split`
#' 
#' @details 
#' 
#' Function [splitKey] finds out whether each keyword appears in each element of input `x`.
#' 
#' `NA_character_` or `''` entries in input `x` are regarded 
#' as negative (i.e., none of the keywords exists), 
#' instead of as missingness (i.e., we do not know if any of the keywords exists).  
#' This practice is most intuitive to clinicians.
#' 
#' 
#' @returns 
#' 
#' Function [splitKey] returns a \link[base]{logical} \link[base]{matrix} if `assign2parent = FALSE`.
#' Otherwise the \link[base]{logical} \link[base]{vector}s are assigned to the parent frame 
#' (i.e., when used inside \link[base]{within.data.frame}).
#' 
#' @examples 
#' 
#' letters[1:4] |> splitKey(split = ';;', envir = NULL) # exception
#' 
#' x = c('a,b,', 'c,a,b,,', NA_character_, '', 'a,b,a')
#' x |> splitKey(split = ',', envir = NULL)
#' 
#' data.frame(x) |> within.data.frame(expr = splitKey(x, split = ','))
#' 
#' data.frame(x) |> within.data.frame(expr = splitKey(x, split = ',', data.name = 'cancer'))
#' 
#' if (FALSE) {
#' library(microbenchmark)
#' X = rep(x, times = 10L)
#' microbenchmark( # speed O(n)
#'  splitKey(x, split = ',', envir = NULL), 
#'  splitKey(X, split = ',', envir = NULL))
#' }
#' @name split_ext
#' @export
splitKey <- function(
    x, key = xkey, 
    data.name = substitute(x),
    envir = parent.frame(),
    ...
) {
  
  if (!is.vector(x, mode = 'character')) stop('input must be character vector') 
  nx <- length(x)
  if (!nx) return(invisible())
  xok <- (!is.na(x) & nzchar(x))
  if (!any(xok)) return(invisible())
  
  xs <- x[xok] |> 
    strsplit(...) |>
    lapply(FUN = trimws_) |>
    lapply(FUN = function(x) x[nzchar(x)]) |>
    lapply(FUN = unique.default) # tolerate duplicates (although they should not be there)
  if (anyNA(xs, recursive = TRUE)) stop('should not happen')
  
  xkey <- xs |>
    unlist(recursive = FALSE, use.names = FALSE) |>
    unique.default() |>
    sort.int() # `key` will not have NA_character_
  if (!missing(key)) {
    id1 <- is.na(match(xkey, table = key, nomatch = NA_integer_))
    if (any(id1)) cat(sQuote(xkey[id1]), 'exist(s) in the data but not in user-provided `key`.\n') 
    id2 <- is.na(match(key, table = xkey, nomatch = NA_integer_))
    if (any(id2)) cat(sQuote(key[id2]), 'exist(s) in user-provided `key` but not in the data.\n')
  }
  
  ret <- array(FALSE, dim = c(nx, length(key)), dimnames = list(x, key)) 
  # \link[base]{matrix} allows duplicated rownames (\link[base]{data.frame} does not)
  ret[xok, ] <- xs |> lapply(FUN = `%in%`, x = key) |> do.call(what = rbind) # speed O(n)
  if (anyNA(ret)) stop('should not happen')
  
  if (is.null(envir) || isFALSE(envir)) return(ret) # stopifnot(length(new.env()) == 0L); cannot use `!length(env)` !!!
  if (!is.environment(envir)) stop('`envir` is not an environment')
  
  if (is.language(data.name)) data.name <- deparse1(data.name) 
  if (!identical(data.name, make.names(data.name))) stop('use syntactically valid `data.name`')
  
  nm <- paste0(data.name, '_', key)
  for (k in rev.default(seq_along(key))) {
    assign(x = nm[k], value = ret[, k], envir = envir)
    # rownames of `ret` does not bother \link[base]{within.data.frame}
  }
  return(invisible())
  
}





#' @rdname split_ext
#' 
#' @returns 
#' Function [splitOrd] returns a \link[base]{logical} \link[base]{matrix} if `envir = NULL`.
#' Otherwise the \link[base]{logical} \link[base]{vector}s are assigned to the `envir`
#' (i.e., when used inside \link[base]{within.data.frame}).
#' 
#' @examples 
#' x = c('T2,N0,M0,B1', 'T4,N0, ,B0', 'T2,N0,M0,B0', 'T2,N1,M0,B0', ',Nx,M0,B0',
#'   'T4,N3,M0,B2', NA, 'T4,N1,M0,B2')
#' nm = c('T', 'N', 'M', 'B')
#' splitOrd(x, split = ',', nm = nm, envir = NULL)
#'   
#' data.frame(x) |> within.data.frame(expr = {
#'   splitOrd(x, split = ',', nm = nm, data.name = 'Stage')
#' })
#'   
#' @export
splitOrd <- function(
    x, nm = stop('must specify new names'),
    data.name = substitute(x),
    envir = parent.frame(),
    ...
) {
  
  if (!is.vector(x, mode = 'character')) stop('input must be character vector') 
  nx <- length(x)
  if (!nx) return(invisible())
  xok <- (!is.na(x) & nzchar(x))
  if (!any(xok)) return(invisible())
  
  xs <- x[xok] |> 
    strsplit(...) |>
    lapply(FUN = trimws_)
  if (anyNA(xs, recursive = TRUE)) stop('should not happen')
  ns <- lengths(xs, use.names = FALSE)
  if (!all(duplicated.default(ns)[-1])) stop('Not all elements split to the same length')
  if (ns[[1L]] == 1L) stop('No split is performed by ?base::strsplit.  Check `split` parameter')
  
  if (!is.character(nm) || anyNA(nm) || !all(nzchar(nm)) || length(nm) != ns[[1L]]) stop('Illegal new names `nm`')
  if (!identical(nm, make.names(nm))) stop('must use syntactically valid `nm`')
  
  ret <- array(NA_character_, dim = c(nx, ns[1L]), dimnames = list(NULL, nm))
  ret[xok,] <- do.call(rbind, args = xs)
  ret[!nzchar(ret)] <- NA_character_
  
  if (is.null(envir) || isFALSE(envir)) return(ret) # stopifnot(length(new.env()) == 0L); cannot use `!length(env)` !!!
  if (!is.environment(envir)) stop('`envir` is not an environment')
  
  if (is.language(data.name)) data.name <- deparse1(data.name) 
  if (!identical(data.name, make.names(data.name))) stop('use syntactically valid `data.name`')
  
  nm <- paste0(data.name, '_', nm)
  for (k in rev.default(seq_len(ns[1L]))) {
    assign(x = nm[k], value = ret[, k], envir = envir)
    # rownames of `ret` does not bother \link[base]{within.data.frame}
  }
  return(invisible())
  
}








