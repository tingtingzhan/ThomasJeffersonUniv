

#' @title Data Inspection
#' 
#' @description ..
#' 
#' @param x \link[base]{data.frame}
#' 
#' @param row_dup_rm \link[base]{logical} scalar, whether to remove duplicated rows.
#' Default `TRUE`.
#' 
#' @param col_na_rm \link[base]{logical} scalar, whether to remove all-missing columns.
#' Default `TRUE`.
#' 
#' @param ptn_Date \link[base]{regex},
#' regular expression pattern of names of the columns to be converted to \link[base]{Dates}.
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' \itemize{
#' \item {convert all eligible \link[base]{integer}, \link[base]{numeric}, 
#' \link[base]{factor} and \link[base]{character} columns to \link[base]{logical}.}
#' }
#' 
#' @note
#' Be aware of potential name clash, e.g., `lavaan::inspect`.
#' 
#' @returns 
#' Function [inspect_] returns (invisibly) a \link[base]{data.frame}.
#' 
#' @export
inspect_ <- function(
    x, 
    row_dup_rm = TRUE, 
    col_na_rm = TRUE,
    ptn_Date, 
    ...
) {
  
  x <- as.data.frame(x) # ?tibble:::as.data.frame.tbl_df, for returned object of ?readxl::read_excel
  
  # remove all-NA rows
  if (any(row_allNA <- (rowMeans(is.na(x)) == 1))) {
    message(sprintf(fmt = '%d all-NA rows removed.', sum(row_allNA)))
    x <- x[!row_allNA, , drop = FALSE]
  }
  
  id <- duplicated.data.frame(x)
  if (any(id)) {
    matchDF(x, trace_duplicate = TRUE) # only to print the ?base::message
    if (row_dup_rm) {
      message(sprintf(fmt = '%d duplicated rows removed.\n', sum(id)))
      x <- x[!id, , drop = FALSE] # `drop` needed for 1-column data.frame!
    } # else message('')
  }

  id <- vapply(x, FUN = function(i) all(is.na(i)), FUN.VALUE = NA)
  if (any(id)) {
    if (col_na_rm) {
      #message(sum(id), ' all-missing columns removed: ', paste(sQuote(head(names(x)[id])), collapse = ', '))
      message(sum(id), ' all-missing columns removed')
      x <- x[!id]
    }
  }
  
  nm <- names(x)
  
  x[] <- lapply(x, FUN = function(i) {
    if (is.character(i)) {
      i <- trimws_(i)
      i[!nzchar(i)] <- NA_character_
      return(i)
    }
    return(i)
  })
  
  if (!missing(ptn_Date)) {
    if (!is.character(ptn_Date) || length(ptn_Date) != 1L || is.na(ptn_Date) || !nzchar(ptn_Date)) stop('illegal ptn_Date')
    cid <- grepl(pattern = ptn_Date, x = nm)
    if (any(cid)) {
      x[cid] <- lapply(nm[cid], FUN = function(inm) {
        # (inm = nm[cid][1L])
        i <- x[[inm]]
        if (is.factor(i)) .Defunct(msg = '?base::data.frame now has default argument `stringsAsFactors = FALSE`')
        if (is.logical(i)) stop('`logical` cannot be converted to `Date`')
        
        if (is.numeric(i)) {
          return(as.Date.numeric(i, origin = '1960-01-01')) # SAS processing
          #stop(sQuote(inm), ' is `numeric`')
          #message(sQuote(inm), ' is `numeric`')
          #return(i)
        }
        
        if (inherits(i, what = c('Date'))) return(i)
        
        if (inherits(i, what = c('POSIXt'))) {
          if (inherits(i, what = 'POSIXct')) i <- as.POSIXlt.POSIXct(i)
          i_ <- unclass(i)
          if (any(i_$hour != 0, na.rm = TRUE)) stop('non-zero hour: ', sQuote(inm))
          if (any(i_$min != 0, na.rm = TRUE)) stop('non-zero min: ', sQuote(inm))
          if (any(i_$sec != 0, na.rm = TRUE)) stop('non-zero sec: ', sQuote(inm))
          return(as.Date.POSIXlt(i))
        }
        
        if (is.character(i)) {
          tmp <- tryCatch(as.Date.character(i, tryFormats = c('%m/%d/%y', '%m-%d-%y', '%m/%d/%Y', '%m-%d-%Y', '%Y-%m-%d')), error = identity)
          if (inherits(tmp, what = 'error')) {
            tmp$message <- paste0(sQuote(inm), ': ', tmp$message)
            # stop(tmp)
            message(conditionMessage(tmp))
            return(i)
          }
          return(tmp)
        }
        
        stop('shouldnt come here')
      })
    } # else do nothing
  } # else do nothing
  
  # x <- inspect_POSIXct(x)
  
  x[] <- lapply(x, FUN = force_bool)
  
  # copy tzh::class1List
  cl1 <- vapply(x, FUN = function(x) class(x)[1L], FUN.VALUE = '', USE.NAMES = TRUE)
  cl2 <- split.default(names(cl1), f = factor(cl1))
  cls <- lapply(cl2, FUN = function(i) {
    if (length(i) < 6L) return(i)
    c(i[1:6], 'etc.')
  })
  names(cls) <- sprintf(fmt = '%d %s', lengths(cl2, use.names = FALSE), names(cl2))
  cat(format_named(cls), sep = '\n')
  return(invisible(x))
}





not_Date <- function(x) {
  if (is.factor(x)) .Defunct(msg = '?base::data.frame now has default argument `stringsAsFactors = FALSE`')
  n <- length(x)
  ret <- logical(length = n) # all-FALSE
  if (!n) return(ret)
  
  if (inherits(x, what = c('Date', 'POSIXt'))) return(ret)
  
  if (is.logical(x)) stop('`logical` cannot be converted to `Date`')
  
  if (is.character(x)) {
    x <- trimws_(x)
    x[!nzchar(x)] <- NA_character_
    return(as.Date.character(x, tryFormats = c('%m/%d/%y', '%m-%d-%y', '%m/%d/%Y', '%m-%d-%Y')))
  }
  
  if (is.numeric(x)) {
    # return() # ???
    # x < 40000 # my old code
    # as.Date.numeric(x, origin = '1899-12-30')
    stop('debug here!!')
  }
  
  stop('shouldnt come here')
}

# ?lubridate::is.Date is much slower than ?base::inherits






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






