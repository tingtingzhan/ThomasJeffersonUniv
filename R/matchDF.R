
#' @title Match Rows of A \link[base]{data.frame} to Another
#' 
#' @description
#' To \link[base]{match} the rows of one \link[base]{data.frame}
#' to the rows of another \link[base]{data.frame}.
#' 
#' @param x \link[base]{data.frame}, the rows of which to be matched.
#' 
#' @param table \link[base]{data.frame}, the rows of which to be matched *against*.
#' 
#' @param by \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param by.x,by.table \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param view.table (optional) \link[base]{character} scalar or \link[base]{vector},
#' variable names of `table` to be printed in fuzzy suggestion (if applicable)
#'  
#' @param trace_nomatch \link[base]{logical} scalar, to provide detailed diagnosis information, default `FALSE`
#' 
#' @param inspect_fuzzy \link[base]{logical} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' The function [matchDF()] returns a \link[base]{integer} \link[base]{vector}
#' 
#' @note
#' Unfortunately, R does not provide case-insensitive \link[base]{match}.
#' Only case-insensitive \link[base]{grep} methods are available.
#' 
#' @examples
#' Formaldehyde[sample(nrow(Formaldehyde), size = 20, replace = TRUE), ] |>
#'   matchDF()
#' @keywords internal
#' @importFrom stringdist stringdist
#' @importFrom utils write.csv
#' @export
matchDF <- function(
    x, 
    table = unique.data.frame(x),
    by = names(x), by.x = character(), by.table = character(),
    view.table = character(),
    trace_nomatch = FALSE,
    inspect_fuzzy = FALSE,
    ...
) {
  
  if (!is.data.frame(x)) stop('`x` must be data.frame')
  
  tab <- table; table <- NULL # dont want to confuse with ?base::table
  if (!is.data.frame(tab)) stop('`table` must be data.frame')
  
  nm.x <- names(x)
  nm.tab <- names(tab)
  
  by.x <- unique.default(c(by, by.x))
  by.tab <- unique.default(c(by, by.table))
  if (any(id <- is.na(match(by.x, table = nm.x)))) stop('Colnames ', paste(sQuote(by.x[id]), collapse = ','), ' absent from `x`')
  if (any(id <- is.na(match(by.tab, table = nm.tab)))) stop('Colnames ', paste(sQuote(by.tab[id]), collapse = ','), ' absent from `table`')
  
  nby <- length(by.x)
  if (nby != length(by.tab)) stop('`by.x` and `by.table` must be same length')
  
  #nm_x <- setdiff(nm.x, by.x)
  #nm_table <- setdiff(nm.tab, by.tab)
  #if (length(nm_ <- intersect(nm_x, nm_table))) stop('do not allow same colnames ', paste(sQuote(nm_), collapse = ','), ' in `x` and `table` (except for `by`)')
  
  x0 <- x[by.x]; .rowNamesDF(x0) <- NULL
  tab0 <- tab[by.tab]; names(tab0) <- by.x; .rowNamesDF(tab0) <- NULL
  # otherwise, if `!identical(by.x, by.tab)`, ?base::match wont work
  if (anyDuplicated.data.frame(tab0)) stop('do not allow duplicated ', sQuote(paste0(by.tab, collapse = '+')), ' in `table`')
  
  id <- match(x = rsplit_(x0), table = rsplit_(tab0), nomatch = NA_integer_)
  id |> show_match() |> print()
  
  if (any(na1 <- is.na(id))) { # rows without a match
    
    x_ <- x0[na1, , drop = FALSE]
    x_uid <- !duplicated.data.frame(x_)
    
    x_u <- x_[x_uid, , drop = FALSE]
    
    for (i in c(rev.default(seq_len(nby - 1L)), 0L)) { # (i = nby - 1L)
      if (i == 0L) break # to indicate nothing full-match
      iseq <- seq_len(i)
      idx <- match(x = rsplit_(unique.data.frame(x_u[iseq])), 
                   table = rsplit_(unique.data.frame(tab0[iseq])), 
                   nomatch = NA_integer_)
      idok <- !is.na(idx)
      if (trace_nomatch) {
        sprintf(
          fmt = '\u2756 Matched %d/%d by %s and %s', 
          sum(idok), 
          length(idx), 
          by.x[iseq] |> paste(collapse = ':') |> col_magenta(), 
          by.tab[iseq] |> paste(collapse = ':') |> col_magenta()
        ) |> message()
      }
      if (all(idok)) break
    }
    
    if (i == 0L) {
      x_dx <- x_u
      tab_dx <- tab0
    } else {
      x_dx <- x_u[, -seq_len(i), drop = FALSE]
      tab_dx <- tab0[, -seq_len(i), drop = FALSE]
    }
    
    min_dist_0 <- lapply(seq_len(length(x_dx)), FUN = \(i) { # (i = 1L)
      tmp <- lapply(x_dx[[i]], FUN = stringdist, b = tab_dx[[i]], method = 'lcs')
      vapply(tmp, FUN = which.min, FUN.VALUE = 0L, USE.NAMES = FALSE)
    })
    min_dist_1 <- .mapply(FUN = c, dots = min_dist_0, MoreArgs = NULL)
    min_dist <- lapply(min_dist_1, FUN = unique.default)
    
    view_table <- if (length(view.table)) tab[view.table] else tab
    fuzzy_suggest <- data.frame(
      x_dx[rep(seq_along(min_dist), times = lengths(min_dist)), , drop = FALSE],
      view_table[unlist(min_dist, use.names = FALSE), , drop = FALSE]
    )
    fuzzy_csv <- tempfile(pattern = 'fuzzy_', fileext = '.csv')
    sprintf(
      fmt = '\u261e %s %d (%d unique) %s having no exact match to %s\n', # extra line feed!!
      fuzzy_csv |> basename() |> col_yellow(),
      sum(na1), 
      sum(x_uid), 
      by.x |> paste(collapse = ':') |> col_magenta(), 
      by.tab |> paste(collapse = ':') |> col_magenta()
    ) |> message()
    if (inspect_fuzzy) {
      write.csv(x = fuzzy_suggest, file = fuzzy_csv, row.names = FALSE)
      paste0('open ', dirname(fuzzy_csv)) |> system()
    }
    
    id_agree <- (lengths(min_dist, use.names = FALSE) == 1L)
    if (any(id_agree)) {  
      #stop('Create REDCap file for Ayako')
      # think what to do next
    }
    
  } # rows without a match
  
  attr(id, which = 'by.x') <- by.x
  attr(id, which = 'by.table') <- by.tab
  return(id)
  
}






#' @title An Alternative Merge Operation
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{data.frame}, on which new columns will be added.
#' All rows of `x` will be retained in the returned object, *in their original order*.
#' 
#' @param table \link[base]{data.frame}, columns of which will be added to `x`.
#' Not all rows of `table` will be included in the returned object
#' 
#' @param by \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param by.x,by.table \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param ... additional parameters of [matchDF]
#' 
#' @note
#' We avoid \link[base]{merge.data.frame} as much as possible,
#' because it's slow and 
#' even `sort = FALSE` may not completely retain the original order of input `x`.
#' 
#' @returns 
#' The function [mergeDF()] returns a \link[base]{data.frame}.
#' 
#' @examples
#' # examples inspired by ?merge.data.frame 
#' (authors = data.frame(
#'  surname = c('Tukey', 'Venables', 'Tierney', 'Ripley', 'McNeil'),
#'  nationality = c('US', 'Australia', 'US', 'UK', 'Australia'),
#'  deceased = c('yes', rep('no', 4))))
#' (books = data.frame(
#'  name = c('Tukey', 'Venables', 'Tierney', 'Ripley', 
#'   'Ripley', 'McNeil', 'R Core', 'Diggle'),
#'  title = c(
#'   'Exploratory Data Analysis',
#'   'Modern Applied Statistics',
#'   'LISP-STAT', 'Spatial Statistics', 'Stochastic Simulation',
#'   'Interactive Data Analysis', 'An Introduction to R',
#'   'Analysis of Longitudinal Data'),
#'  other.author = c(
#'   NA, 'Ripley', NA, NA, NA, NA, 'Venables & Smith',
#'   'Heagerty & Liang & Scott Zeger')))
#' (m = mergeDF(books, authors, by.x = 'name', by.table = 'surname'))
#' attr(m, 'nomatch')
#' 
#' @keywords internal
#' @export
mergeDF <- function(
    x, table, 
    by = character(), by.x = character(), by.table = character(),
    ...
) {
  
  id <- matchDF(x = x, table = table, by = by, by.x = by.x, by.table = by.table, ...)
  by.x <- attr(id, which = 'by.x', exact = TRUE)
  by.table <- attr(id, which = 'by.table', exact = TRUE)
  
  nm_table <- setdiff(names(table), by.table)
  if (length(nm_ <- intersect(
    x = setdiff(names(x), by.x), 
    y = nm_table
  ))) stop('do not allow same colnames ', 
           nm_ |> paste(collapse = ':') |> col_magenta(), 
           ' in `x` and `table` (except for `by`)')
  
  ret <- data.frame(x, table[id, nm_table, drop = FALSE])
  rownames(ret) <- rownames(x) # otherwise be overriden by rownames(table[...])
  return(ret)
  
}





#' @title Split \link[base]{data.frame} into Individual Rows
#' 
#' @description
#' \link[base]{split.data.frame} into individual rows.
#' 
#' @param x a \link[base]{data.frame}
#' 
#' @returns
#' The function [rsplit_()] returns a \link[base]{list} of \link[base]{nrow}-1 \link[base]{data.frame}s.
#' 
#' @seealso 
#' \link[base]{split.data.frame};
#' \link[base]{anyDuplicated.data.frame}.
#' 
#' @examples
#' datasets::USArrests |> head(n = 3L) |> rsplit_() # with rownames
#' datasets::Formaldehyde |> rsplit_() # without rownames
#' data.frame() |> rsplit_() # exception
#' 
#' @keywords internal
#' @export
rsplit_ <- function(x) {
  x |>
    .row_names_info(type = 2L) |> 
    seq_len() |>
    lapply(FUN = \(i) x[i, , drop = FALSE])
}





#' @importFrom english ordinal
#' @importFrom flextable flextable autofit vline
show_match <- function(x) {
  
  if (!anyDuplicated.default(x)) return(invisible())
  
  tmp <- x |> 
    seq_along() |>
    split.default(f = factor(x))
  id <- (lengths(tmp, use.names = FALSE) > 1L) |> which()
  
  data.frame(
    'Unique Element' = paste(id, id |> ordinal(), sep = '; '),
    'Appear at Location' = tmp[id] |> 
      vapply(FUN = paste, collapse = ', ', FUN.VALUE = NA_character_),
    'Excel Row (+1)' = tmp[id]|> 
      lapply(FUN = `+`, 1L) |> 
      vapply(FUN = paste, collapse = ', ', FUN.VALUE = NA_character_),
    check.names = FALSE
  ) |> 
    flextable() |>
    autofit() |>
    vline(j = 1:2)
  
}
