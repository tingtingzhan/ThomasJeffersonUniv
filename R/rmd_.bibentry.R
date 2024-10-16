

#' @title R Markdown Format of \link[utils]{citation} and/or \link[utils]{bibentry}
#' 
#' @description 
#' R markdown format of a \link[utils]{citation} and/or \link[utils]{bibentry} object.
#' 
#' @param x a \link[utils]{citation} and/or \link[utils]{bibentry} object
#'
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Function [rmd_.bibentry] beautifies the output from 
#' function `utils:::format.bibentry` (with option `style = 'text'`)
#' in the following ways.
#' \itemize{
#' \item{Line break `'\n'` is replaced by a white space;}
#' \item{Fancy quotes \eqn{``}, \eqn{''}, \eqn{`} and \eqn{'} are removed;}
#' \item{doi entries are shown as URLs with labels (in R markdown grammar).}
#' }
#' 
#' @returns 
#' Function [rmd_.bibentry] returns a \link[base]{character} scalar or \link[base]{vector}.
#' 
#' @examples 
#' rmd_.bibentry(citation())
#' rmd_.bibentry(citation('survival'))
#' if (FALSE) { # disabled for ?devtools::check
#' ap = rownames(installed.packages())
#' lapply(ap, FUN = function(i) rmd_.bibentry(citation(i)))
#' }
#' @importFrom stringi stri_extract_all_regex stri_replace_all_fixed stri_replace_all_regex
#' @export rmd_.bibentry
#' @export
rmd_.bibentry <- function(x, ...) {
  
  y <- format(x, style = 'text') # ?utils:::format.citation -> ?utils:::format.bibentry
  # return may have `length(y) > 1L`
  if (!length(y) || anyNA(y) || any(!nzchar(y))) stop('Package ', sQuote(x), ' updated?')
  
  y <- gsub(pattern = '\n', replacement = ' ', x = y)
  
  # '\u201c|\u201d' # quotation marks created by ?base::dQuote
  # '\u2018|\u2019' # quotation marks created by ?base::sQuote
  y <- gsub(pattern = '\"|\u201c|\u201d|\u2018|\u2019', replacement = '', x = y)
  
  if (TRUE) {
    ptn <- '( doi:)(.*?)( <https://doi.org/)(.*?)(>)(.|,)'
    doi <- stri_extract_all_regex(str = y, pattern = ptn)
    doi_pattern <- c(' doi:', ' <https://doi.org/', '>')
    doi_replacement <- c(' [doi:', '](https://doi.org/', ')')
    y <- unlist(.mapply(FUN = function(y_, doi_) {
      doi_new <- stri_replace_all_fixed(str = doi_, pattern = doi_pattern, replacement = doi_replacement, vectorize_all = FALSE) # to get [topic](url)
      stri_replace_all_regex(str = y_, pattern = ptn, replacement = doi_new)  
    }, dots = list(y_ = y, doi_ = doi), MoreArgs = NULL), use.names = FALSE)
  } # [topic](url) for doi, in R markdown grammar
  
  pkg <- attr(x, which = 'package', exact = TRUE)
  
  return(sprintf(
    fmt = '<u>**`%s`**</u> %s\n', 
    if (pkg == 'base') 'R' else pkg, 
    y))
  
}




