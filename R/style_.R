


#' @title Highlight Style for Sample Size
#' 
#' @param x \link[base]{integer} scalar
#' 
#' @returns 
#' Function [style_samplesize()] returns a \link[base]{character} scalar.
#' 
#' @examples
#' 30L |> style_samplesize() |> cat()
#' 30L |> style_samplesize() |> message()
#' 
#' @keywords internal
#' @export
style_samplesize <- function(x) {
  x |> bg_br_yellow() |> style_bold()
}



#' @title Highlight Style for File Base Name
#' 
#' @param x \link[base]{character} scalar
#' 
#' @returns 
#' Function [style_basename()] returns a \link[base]{character} scalar.
#' 
#' @examples
#' './a/b.R' |> style_basename() |> cat()
#' './a/b.R' |> style_basename() |> message()
#' @keywords internal
#' @export
style_basename <- function(x) {
  x |> basename() |> col_yellow()
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
#' letters[1:3] |> style_interaction() |> cat()
#' letters[1:3] |> style_interaction() |> message()
#' style_interaction(~ mrn + dob) |> cat()
#' 
#' @keywords internal
#' @export
style_interaction <- function(x) {
  if (is.call(x) && x[[1L]] == '~') {
    x <- all.vars(if (length(x) == 2L) x[[2L]] else x[[3L]])
  }
  if (!is.character(x)) stop('x must be convertible to `character`')
  x |> paste(collapse = ':') |> col_magenta()
}
