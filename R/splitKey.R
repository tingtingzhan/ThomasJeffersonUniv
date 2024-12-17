

#' @title Split \link[base]{character} \link[base]{vector} into Keywords
#' 
#' @description ..
#' 
#' @param x \link[base]{character} \link[base]{vector}, each element being a collection of 
#' keywords separated by a symbol (e.g., `','`)
#' 
#' @param keys (optional) \link[base]{character} \link[base]{vector}, user-specified 
#' keywords.  Default to all keywords appearing in input `x`
#' 
#' @param data.name (optional) \link[base]{character} scalar or \link[base]{name},
#' name of input `x`
#' 
#' @param assign2parent \link[base]{logical} scalar, whether to assign the result to the parent frame
#' (i.e., when used inside \link[base]{within.data.frame}).  Default `TRUE`
#' 
#' @param ... potential parameters of \link[base]{strsplit}, most importantly `split`
#' 
#' @details 
#' 
#' Function [splitKey] finds out whether each keyword appears in each element of input `x`.
#' 
#' `NA_character_` or `''` entries in input `x` are regarded 
#' as negative (i.e., none of the key words exists), 
#' instead of as missingness (i.e., we do not know if any of the keywords exists).  
#' This practice is most intuitive to clinicians.
#' 
#' @note
#' 
#' It is presumed there is few duplication in the input `x`.
#' 
#' @returns 
#' 
#' Function [splitKey] returns a \link[base]{logical} \link[base]{matrix} if `assign2parent = FALSE`.
#' Otherwise the \link[base]{logical} \link[base]{vector}s are assigned to the parent frame 
#' (i.e., when used inside \link[base]{within.data.frame}).
#' 
#' @examples 
#' 
#' x = letters[1:4]
#' splitKey(x, split = ';;', assign2parent = FALSE)
#' 
#' x = c('a,b,', 'c,a,b,,', NA_character_, '', 'a,b,')
#' splitKey(x, split = ',', assign2parent = FALSE)
#' 
#' within(data.frame(x), expr = splitKey(x, split = ','))
#' 
#' within(data.frame(x), expr = splitKey(x, split = ',', data.name = 'cancer'))
#' 
#' if (FALSE) {
#' library(microbenchmark)
#' X = rep(x, times = 1e1L)
#' microbenchmark( # speed O(n)
#'  splitKey(x, split = ',', assign2parent = FALSE), 
#'  splitKey(X, split = ',', assign2parent = FALSE))
#' }
#' 
#' @export
splitKey <- function(
    x, keys = xkeys, 
    data.name = substitute(x),
    assign2parent = TRUE,
    ...
) {
  
  if (!is.vector(x, mode = 'character')) stop('input must be character vector') 
  if (!(nx <- length(x))) return(invisible())
  xok <- (!is.na(x) & nzchar(x))
  if (!any(xok)) return(invisible())
  
  xs_ <- strsplit(x[xok], ...)
  #if (all(lengths(xs_) == 1L)) stop('I now allow this')
  if (anyNA(xs_, recursive = TRUE)) stop('?base::strsplit does not give NA output')
  
  xs <- lapply(xs_, FUN = function(ix) {
    ix <- trimws_(ix)
    return(unique.default(ix[nzchar(ix)])) # tolerate duplicates (although they should not be there)
  })
  
  xkeys <- sort.int(unique.default(unlist(xs, recursive = FALSE, use.names = FALSE))) # `keys` will not have NA_character_
  if (!missing(keys)) {
    id1 <- is.na(match(xkeys, table = keys, nomatch = NA_integer_))
    if (any(id1)) cat(sQuote(xkeys[id1]), 'exist(s) in the data but not in user-provided `keys`.\n') 
    id2 <- is.na(match(keys, table = xkeys, nomatch = NA_integer_))
    if (any(id2)) cat(sQuote(keys[id2]), 'exist(s) in user-provided `keys` but not in the data.\n')
  }
  
  nk <- length(keys)
  ret <- array(FALSE, dim = c(nx, nk), dimnames = list(x, keys)) 
  # default is FALSE, instead of NA
  # \link[base]{matrix} allows duplicated rownames (\link[base]{data.frame} does not)
  ret[xok, ] <- do.call(rbind, args = lapply(xs, FUN = `%in%`, x = keys)) # speed O(n)
  if (anyNA(ret)) stop('should not happen')
  
  if (!assign2parent) return(ret)
  
  data.name <- if (is.symbol(data.name)) {
    deparse1(data.name) 
  } else if (is.character(data.name) && length(data.name) == 1L) {
    data.name
  } else stop('`data.name` must be convertible to len-1 character')
  
  parent.env <- parent.frame()
  nms <- paste0(data.name, '_', keys)
  for (k in rev.default(seq_along(keys))) {
    assign(x = nms[k], value = ret[, k], envir = parent.env)
    # rownames of `ret` does not bother \link[base]{within.data.frame}
  }
  return(invisible())
  
}


