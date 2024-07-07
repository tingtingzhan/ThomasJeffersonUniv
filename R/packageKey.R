


#' @title Use \link[utils]{packageName} as \link[utils]{citation} Key
#' 
#' @description 
#' 
#' Use \link[utils]{packageName} as \link[utils]{citation} key
#' 
#' @param x \link[utils]{citation} object
#' 
#' @param overwrite \link[base]{logical} scalar, whether to overwrite
#' default key(s).  Default `FALSE`.
#' 
#' @details 
#' 
#' Function [packageKey] adds \link[utils]{packageName} as \link[utils]{citation} key.
#' 
#' @returns 
#' 
#' Function [packageKey] returns a \link[utils]{citation} object.
#' 
#' @note
#' As of June 2024:
#' if the last call to \link[utils]{bibentry} in 
#' function \link[utils]{citation} adds the argument `key = package`, 
#' or provide a parameter to allow end-user to enable such choice,
#' then we don't need the function [packageKey].
#' 
#' @examples
#' if (FALSE) {
#'  ap = installed.packages()
#'  table(aa[, 'Priority'])
#'  which(ap[, 'Priority'] == 'recommended')
#' }
#' 
#' toBibtex(ct <- citation('survival')) # has default key(s)
#' toBibtex(packageKey(ct))
#' toBibtex(packageKey(ct)[1L])
#' toBibtex(packageKey(ct, overwrite = TRUE)[2L])
#' 
#' toBibtex(ct <- citation()) # no default key(s)
#' toBibtex(packageKey(ct))
#' @export
packageKey <- function(x, overwrite = FALSE) {
  
  if (!inherits(x, what = 'citation')) stop('input must be `citation`')
  pkg <- attr(x, which = 'package', exact = TRUE) # see ?utils:::.citation
  
  if (!overwrite) {
    key0 <- lapply(unclass(x), FUN = attr, which = 'key', exact = TRUE)
    if (all(lengths(key0, use.names = FALSE) > 0L)) return(x)
  }
  
  bibentry_key(x) <- pkg
  return(x)
  
}





#' @title Assign Key to \link[utils]{bibentry}
#' 
#' @description
#' Assign an \link[base]{attributes} named `'key'` to \link[utils]{bibentry} object.
#' 
#' @param x \link[utils]{bibentry} (and inherited \link[utils]{citation}) object 
#' 
#' @param value \link[base]{character} scalar or \link[base]{vector}
#' 
#' @note 
#' `'key'` is the attribute of \link[utils]{bibentry}, which is used in 
#' `utils:::.bibentry_get_key` and `utils:::toBibtex.bibentry`.
#' 
#' @export
`bibentry_key<-` <- function(x, value) {
  
  if (!inherits(x, what = 'bibentry')) stop('input must be bibentry object')
  
  if (!(nx <- length(x))) stop('input is len-0')
  nv <- length(value)
  
  if (nx > 1L && nv == 1L) {
    value <- paste(value, seq_len(nx), sep = '_')
  } else if (nx != nv) stop('length of x and value do not match')
  
  ret <- .mapply(`attr<-`, dots = list(x = unclass(x), value = value), MoreArgs = list(which = 'key'))
  # ?base::`attr<-` is *not* S3 generic!!
  class(ret) <- class(x)
  return(ret)
  
}






if (FALSE) {
  # study ?utils:::`[.bibentry` carefully!!
  identical(utils:::`[.bibentry`, utils:::`[[.bibentry`)
}

