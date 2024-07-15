


#' @title File Modification Time by Pattern
#' 
#' @description ..
#' 
#' @param path \link[base]{character} scalar, directory on hard drive, see \link[base]{list.files}
#' 
#' @param pattern \link[base]{character} scalar, regular expression \link[base]{regex}
#' 
#' @param file (optional) \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param ... ..
#' 
#' @examples 
#' \dontrun{ # devtools::check error
#' file_mtime('./R', pattern = '\\.R$')
#' file_mtime('./src', pattern = '\\.cpp$')
#' }
#' 
#' @seealso 
#' \link[base]{file.info} \link[base]{file.mtime}
#' 
#' @export
file_mtime <- function(
    path,
    pattern = '\\.xlsx$|\\.xls$|\\.csv$',
    file = list.files(path = path, pattern = pattern, ..., full.names = TRUE), # ?base::list.files do not allow `...`
    ...
) {
  
  if (missing(file)) {
    if (!dir.exists(path)) stop('Specified directory does not exist!')
    force(file)
  }
  if (!length(file)) stop('do not have files with given `pattern`')
  
  # see ?base::file.mtime and ?base::file.info
  # ?tools::file_path_sans_ext can further remove file extension
  tmp <- file.info(file, extra_cols = FALSE)
  id <- !tmp[['isdir']] # see ?utils::file_test
  ret0 <- .POSIXct(tmp$mtime[id]) # 'POSIXct'
  names(ret0) <- basename(file[id])
  ret <- sort.int(ret0, decreasing = TRUE)
  cat(format_named(format(ret), sep = ' @ '), sep = '\n')
  return(invisible(ret))
  
}


