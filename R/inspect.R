

#' @title Data Inspection
#' 
#' @description ..
#' 
#' @param x \link[base]{data.frame}
#' 
#' @param duplicated_rm \link[base]{logical} scalar, whether to remove duplicated *rows*.
#' Default `FALSE`.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' \itemize{
#' \item {convert all eligible \link[base]{integer}, \link[base]{numeric}, 
#' \link[base]{factor} and \link[base]{character} columns to \link[base]{logical}.}
#' }
#' 
#' @export
inspect <- function(x, duplicated_rm = TRUE, ...) {
  
  x <- as.data.frame(x) # ?tibble:::as.data.frame.tbl_df, for returned object of ?readxl::read_excel
  
  # remove all-NA rows
  if (any(row_allNA <- (rowMeans(is.na(x)) == 1))) {
    message(sprintf(fmt = '%d all-NA rows removed.', sum(row_allNA)))
    x <- x[!row_allNA, , drop = FALSE]
  }
  
  id <- duplicated.data.frame(x)
  if (any(id)) {
    matchDF(x, trace = TRUE) # only to print the ?base::message
    if (duplicated_rm) {
      message(sprintf(fmt = '%d duplicated rows removed.\n', sum(id)))
      x <- x[!id, , drop = FALSE] # `drop` needed for 1-column data.frame!
    } else message('')
  }
  
  x <- inspect_Date(x)
  
  # x <- inspect_POSIXct(x)
  
  x[] <- lapply(x, FUN = force_bool)
  
  cls <- class1List(x)
  cls_print <- lapply(cls, FUN = function(i) {
    if (length(i) < 6L) return(i)
    c(i[1:6], 'etc.')
  })
  cat(format_named(cls_print), sep = '\n')
  return(x)
}




# Function \link[lubridate]{is.Date} is much slower than \link[base]{inherits}.


# @param subj_id \link[base]{integer} scalar or \link[base]{vector}, subject column of `x`.
# 
# [inspect_Date] any `'^Date_'` columns read as \link[base]{character} or \link[base]{numeric}
inspect_Date <- function(x, subj_id = 1L) {
  
  subj_nm <- names(x)[subj_id]
  cls <- class1List(x)
  if (length(cls$factor)) stop('Since R 4.0.0, default stringsAsFactors = FALSE')
  
  # could be 'Date' or 'POSIXct'
  if (length(cls$character)) {
    .chr_Date <- startsWith(cls$character, prefix = 'Date_')
    .chr_DateTime <- startsWith(cls$character, prefix = 'DateTime_') 
    .chr <- cls$character[.chr_Date | .chr_DateTime]
    for (ichr in .chr) { # len-0 `.chr` compatible
      ilv <- unique.default(x[[ichr]])
      id_not_dbl <- !type2dbl(ilv)
      if (any(id_not_dbl)) stop('Remove level(s) ', sQuote(ilv[id_not_dbl]), ' in ', sQuote(ichr))
      
      # all below in this `for`: not longer needed (really?)
      
      id_chr <- not_numeric(ilv)
      id_num <- type2dbl(ilv) & !is.na(ilv)
      
      # currently only allow '%m/%d/%y' '%m/%d/%Y' separated by '; |;|, |,'.  1st Date will be retained.
      #iv1 <- .strsplit_id(trimws_(ilv[id_chr]), id = 1L, split = '; |;|, |,') 
      iv1 <- vapply(strsplit(trimws_(ilv[id_chr]), split = '; |;|, |,', fixed = FALSE), FUN = `[`, i = 1L, FUN.VALUE = '')
      #id_start_yr2 <- grepl(pattern = '^[0-9]{2}/|^[0-9]{2}-', x = iv1) # '11/24/15' will be ambiguous!!!
      #id_start_yr4 <- grepl(pattern = '^[0-9]{4}/|^[0-9]{4}-', x = iv1)
      id_end_yr2 <- grepl(pattern = '/[0-9]{2}$|-[0-9]{2}$', x = iv1) # consider replacing '0-9' with '\\d'
      id_end_yr4 <- grepl(pattern = '/[0-9]{4}$|-[0-9]{4}$', x = iv1)
      #id_error <- #!id_start_yr2 & !id_start_yr4 & !id_end_yr2 & !id_end_yr4
      id_error <- !id_end_yr2 & !id_end_yr4
      if (any(id_error)) {
        print.data.frame(x[which(id_chr)[id_error], c(subj_nm, ichr)], row.names = FALSE)
        stop('Check error above!')
      }
      
      ival_new <- .Date(rep(NA_integer_, times = length(ilv)))
      #if (any(id_start_yr2)) ival_new[id_chr][id_start_yr2] <- as.Date.character(iv1[id_start_yr2], tryFormats = c('%y/%m/%d', '%y-%m-%d'))
      #if (any(id_start_yr4)) ival_new[id_chr][id_start_yr4] <- as.Date.character(iv1[id_start_yr4], tryFormats = c('%Y/%m/%d', '%Y-%m-%d'))
      if (any(id_end_yr2)) ival_new[id_chr][id_end_yr2] <- as.Date.character(iv1[id_end_yr2], tryFormats = c('%m/%d/%y', '%m-%d-%y'))
      if (any(id_end_yr4)) ival_new[id_chr][id_end_yr4] <- as.Date.character(iv1[id_end_yr4], tryFormats = c('%m/%d/%Y', '%m-%d-%Y'))
      ival_new[id_num] <- as.Date.numeric(as.double(ilv[id_num]), origin = '1899-12-30')
      x[[ichr]] <- ival_new
    }
    if (length(.chr)) message('Only the 1st date is retained in columns ', sQuote(.chr))
  }

  if (length(cls$numeric)) {
    .num_Date <- startsWith(cls$numeric, prefix = 'Date_')
    .num <- cls$numeric[.num_Date]
    for (i in .num) { # len-0 `.num` compatible
      eval(call(name = 'subset_', quote(x), subset = call(name = '<', as.symbol(i), 40000), select = quote(subj_nm)))
    }
  }
  
  return(x)
  
}





inspect_POSIXct <- function(x) {
  .Defunct(msg = 'Let end user use ?POSIXct2difftime at their own descretion')
  #if (!any(id <- v_inherits(x, what = 'POSIXct'))) return(x)
  #for (i in which(id)) {
  #  x[[i]] <- POSIXct2difftime(x[[i]], else_return = x[[i]])
  #}
  #return(x)
}




#' @title Convert No-Date \link[base]{POSIXct} to \link[base]{difftime}
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{POSIXct} 
#' 
#' @param else_return exception handling
#' 
#' @details
#' \CRANpkg{readxl} will read `'hour:min:sec'` as `'1899-12-31 hr:min:sec UTC'`
#' 
#' @seealso `lubridate:::year.default` `lubridate:::tz.POSIXt`
#' @export
POSIXct2difftime <- function(
    x, 
    else_return = stop('Expecting all \'1899-12-31 hr:min:sec UTC\'')
) {
  tz <- attr(x, which = 'tzone', exact = TRUE)
  if (!length(tz)) stop('really?')
  if (length(tz) > 1L) stop('really?')
  yr <- as.POSIXlt.POSIXct(x, tz = tz)$year
  if (!all(yr == -1, na.rm = TRUE)) return(else_return)
  x - as.POSIXct(strptime('1899-12-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'), tz = tz)
}







#' @title Force to \link[base]{logical} \link[base]{vector}
#' 
#' @description ..
#' 
#' @param x a \link[base]{factor}, or a \link[base]{vector}
#' 
#' @param else_return an R object to return if cannot force into \link[base]{logical}.
#' Default `x`.
#' 
#' @details
#' 
#' Function [force_bool] tries to turn an object into \link[base]{logical}.
#' 
#' @examples 
#' force_bool(c('0', '1', '0', NA))
#' (tmp = factor(rep(0:1, times = 10L)))
#' force_bool(tmp)
#' 
#' @export
force_bool <- function(x, else_return = x) {
  
  # 'factor' ?base::is.atomic but not ?base::is.vector
  if (is.factor(x)) {
    # ?base::is.na, ?base::tolower, ?base::nzchar and ?base::gsub are all O(n) !
    x <- factor(x) # drops duplicates in levels
    xl <- tolower(trimws_(attr(x, which = 'levels', exact = TRUE)))
    # stopifnot(!anyNA(xl)) # !!!
    id <- if (all(xl %in% (str_chk <- c('unchecked', 'checked')))) { # REDCap
      xl == 'checked'
    } else if (all(xl %in% (str_yn <- c('y', 'yes', 'no', 'n', 'true', 'false')))) {
      xl %in% c('yes', 'y', 'true')
    } else if (all(xl %in% (str_yni <- c('yes', 'indeterminate', 'no')))) {
      xl %in% c('yes', 'indeterminate')
    } else if (all(xl %in% (str_np <- c('negative', 'neg', 'positive', 'pos')))) {
      xl %in% c('positive', 'pos')
    } else if (all(xl %in% (str_pm <- c('+', '-', '\u00B1')))) {
      xl %in% c('+', '\u00B1')
    } else if (all(xl %in% (str_01 <- c('0', '1')))) {
      as.logical(as.double(xl))
    } else if (all(xl %in% (str_tf <- c('true', 'false')))) {
      as.logical(xl) # ?base::as.logical handles lower-case true/false correctly
    } else return(else_return)
    return(id[x])
  }
  
  if (!is.atomic(x) || !is.vector(x)) return(else_return) # `NULL` goes here
  class(x) <- setdiff(class(x), y = 'AsIs')
  
  if (is.logical(x)) return(x)
  
  n <- length(x)
  out <- rep(NA, times = n)
  if (!n) return(out) 
  
  xok <- !is.na(x)
  if (!any(xok)) return(out)
  
  if (is.numeric(x)) {
    if (all(x == 0L | x == 1L, na.rm = TRUE)) {
      out[] <- as.logical(x)
      return(out)
    } else return(else_return)
  }
  
  if (is.character(x)) { # recursive!
    return(force_bool(factor(x), else_return = else_return))
  }
  
  return(else_return)
  
}






type2dbl <- function(x) { # element-wise
  .Deprecated(new = '!not_numeri(..)')
  suppressWarnings(x_dbl <- as.double(x))
  is.na(x) | !is.na(x_dbl) # beautiful!
}




#' @title Elements which are not \link[base]{numeric}
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @details
#' Function [not_numeric] finds the elements cannot be handled by 
#' \link[base]{as.numeric} (workhorse \link[base]{as.double}).
#' 
#' @returns 
#' Function [not_numeric] returns a \link[base]{logical} \link[base]{vector}.
#' 
#' @examples
#' not_numeric(c('1.9', '1.1.3', Inf, NA))
#' @export
not_numeric <- function(x) {
  
  if (is.factor(x)) .Defunct(msg = '?base::data.frame now has default argument `stringsAsFactors = FALSE`')
  
  n <- length(x)
  ret <- logical(length = n) # all-FALSE
  if (!n) return(ret)
  
  if (is.logical(x) || is.numeric(x)) return(ret) # ?base::as.double can handle 'logical'
  
  if (is.character(x)) {
    suppressWarnings(x0 <- as.double(x))
    return(xor(is.na(x), is.na(x0)))
  }
  
  stop('shouldnt come here')
  
}






