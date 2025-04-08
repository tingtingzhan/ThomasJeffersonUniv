

#' @title Format \link[base]{file.size}
#' 
#' @description 
#' Format \link[base]{file.size}.
#' 
#' @param ... \link[base]{character} scalar or \link[base]{vector}, file paths
#' 
#' @param units \link[base]{character} scalar, 
#' see parameter `units` of function \link[utils]{format.object_size}
#' 
#' @details
#' Function [format_file_size] formats \link[base]{file.size} 
#' in the same manner as 
#' function \link[utils]{format.object_size} does to \link[utils]{object.size}.
#' 
#' @note
#' Return of function \link[base]{file.size} is simply \link[base]{numeric},
#' thus we will not be able to define an S3 method dispatch for the S3 generic \link[base]{format}, yet.
#' 
#' @returns 
#' Function [format_file_size] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples 
#' # format_file_size('./R/allequal.R', './R/approxdens.R')
#' @export
format_file_size <- function(..., units = 'auto') {
  sz <- file.info(..., extra_cols = FALSE)$size # ?base::file.size
  vapply(sz, FUN = \(i) {
    class(i) <- 'object_size' 
    format(i, units = units) # ?utils:::format.object_size 
  }, FUN.VALUE = '')
}

