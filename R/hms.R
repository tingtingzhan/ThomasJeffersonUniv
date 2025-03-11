
#' @title Hour-Minute-Second
#' 
#' @description
#' Function \link[readxl]{read_excel} might read a date-less time stamp 
#' (e.g., `3:00:00 PM` as `1899-12-31 15:00:00 UTC`).
#' 
#' @param x a \link[base]{POSIXlt} or \link[base]{POSIXct} object
#' 
#' @returns
#' Function [hms()] returns a \link[base]{difftime} object.
#' 
#' @seealso \link[base]{ISOdatetime}
#' @name hms
#' @export
hms <- function(x) UseMethod(generic = 'hms')

#' @rdname hms
#' @export hms.POSIXlt
#' @export
hms.POSIXlt <- function(x) {
  
  if (!all(x$year == -1L, na.rm = TRUE) ||
      !all(x$mon == 11L, na.rm = TRUE) ||
      !all(x$mday == 31L, na.rm = TRUE)) stop('should not use [hms()]')
  
  # sapply(unclass(x), FUN = typeof)
  
  x0 <- x # start of time on that day
  
  int0 <- rep(0L, times = length(x))
  int0[is.na(x)] <- NA_integer_
  x0$hour <- x0$min <- int0
  
  dbl0 <- rep(0, times = length(x))
  dbl0[is.na(x)] <- NA_real_
  x0$sec <- dbl0
     
  difftime(time1 = x, time2 = x0, units = 'auto')
  
}


#' @rdname hms
#' @export hms.POSIXct
#' @export
hms.POSIXct <- function(x) {
  x |> 
    as.POSIXlt.POSIXct() |> 
    hms.POSIXlt()
}