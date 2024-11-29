


#' @title Highlight Style for Sample Size
#' 
#' @param x \link[base]{integer} scalar
#' 
#' @returns 
#' Function [style_samplesize] returns a \link[base]{character} scalar.
#' 
#' @examples
#' cat(style_samplesize(30L))
#' message(style_samplesize(30L))
#' 
#' @keywords internal
#' @export
style_samplesize <- function(x) {
  style_bold(bg_br_yellow(x))
}



#' @title Highlight Style for File Base Name
#' 
#' @param x \link[base]{character} scalar
#' 
#' @returns 
#' Function [style_basename] returns a \link[base]{character} scalar.
#' 
#' @examples
#' cat(style_basename('./a/b.R'))
#' message(style_basename('./a/b.R'))
#' 
#' @keywords internal
#' @export
style_basename <- function(x) {
  col_yellow(basename(x))
}


# @importFrom cli cli_text
#cli_text(sprintf(fmt = '\u261e {.href [%s](file://%s)}', basename(x), normalizePath(x)))
# as of 2024-04, an existing file can be opened **inside RStudio**
# .. RStudio will warn of a non-existing file
# also, I don't know how to base::message cli::cli_text


# if (FALSE) {
# style_basename_FUTURE('~/Desktop/a.pdf')
# style_basename_FUTURE('~/Desktop/b.pdf')
# }
#style_basename_FUTURE <- function(x) {
#  cli::cli_text(sprintf(fmt = '\u261e {.href [%s](file://%s)}', basename(x), normalizePath(x)))
#}



#' @title Highlight Style for (\link[base]{interaction} of) \link[base]{factor}s
#' 
#' @param x \link[stats]{formula} or \link[base]{character} vector
#' 
#' @returns 
#' Function [style_interaction] returns a \link[base]{character} scalar.
#' 
#' @examples
#' cat(style_interaction(letters[1:3]))
#' message(style_interaction(letters[1:3]))
#' cat(style_interaction(~ mrn + dob))
#' 
#' @keywords internal
#' @export
style_interaction <- function(x) {
  if (is.call(x) && x[[1L]] == '~') {
    x <- all.vars(if (length(x) == 2L) x[[2L]] else x[[3L]])
  }
  if (!is.character(x)) stop('x must be convertible to `character`')
  col_magenta(paste(x, collapse = ':'))
}
