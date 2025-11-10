

#' @title Inspect Duplicated Rows in a \link[base]{data.frame}
#' 
#' @description
#' To inspect duplicated rows in a \link[base]{data.frame}.
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param f \link[stats]{formula},
#' criteria of duplication, e.g., 
#' use `~ mrn` to identify duplicated `mrn`, or 
#' use `~ mrn + visitdt` to identify duplicated `mrn:visitdt`
#'
#' @param dontshow (optional) \link[base]{character} scalar or \link[base]{vector},
#' variable names to be omitted in output diagnosis `file`
#' 
#' @param file \link[base]{character} scalar,
#' path of diagnosis file, 
#' print out of substantial duplicates
#' 
#' @param rule \link[base]{language}, rule of dealing with duplicates
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [checkDuplicated()] returns a \link[base]{data.frame}.
#' 
#' @examples
#' fml = Formaldehyde
#' fml$id = seq_len(nrow(fml))
#' (x = fml[c(1:3, 1:6), ])
#' x$optden[4] = x$optden[1] + 1
#' x$carb[2] = x$optden[5L] = NA_real_
#' x$optden[3] = x$carb[6L] = NA_real_
#' x
#' 
#' checkDuplicated(x, ~ id)
#' checkDuplicated(x, ~ id, rule = optden == max(optden))
#' 
#' @keywords internal
#' @importFrom writexl write_xlsx
#' @export
checkDuplicated <- function(
    data, f, 
    dontshow = character(length = 0L),
    file = tempfile(pattern = 'checkdup_', fileext = '.xlsx'),
    rule,
    ...
) {
  
  dup_txt <- f |> all.vars() |> paste(collapse = ':') |> style_bold() |> col_magenta()
  
  # split-ted row-id by `f`
  rid <- data |> 
    .row_names_info(type = 2L) |> 
    seq_len() |>
    split.default(f = interaction(data[all.vars(f)], drop = TRUE, lex.order = TRUE))

  n <- lengths(rid, use.names = FALSE)
  if (any(n == 0L)) stop('wont happen')
  ns <- (n > 1L)
  if (!any(ns)) {
    sprintf(fmt = '\u2714 No duplicated %s\n', dup_txt) |> message()
    return(invisible(data))
  }
  
  ds <- rid[ns] |>
    lapply(FUN = \(i) data[i, , drop = FALSE]) # if too slow, use parallel
  
  ds_coalesce <- ds |>
    lapply(FUN = \(d) {
      # attempt column-wise coalesce?
      tryCatch(expr = lapply(d, FUN = unique_), error = identity)
    })
  
  id_truedup <- ds_coalesce |>
    vapply(FUN = inherits, what = 'error', FUN.VALUE = NA)
  
  d_coalesce <- if (any(!id_truedup)) {
    sprintf(fmt = '%d %s', sum(!id_truedup), dup_txt) |>
      bg_br_green() |>
      sprintf(fmt = '\u2714 %s with coalesce-able duplicates') |> 
      message()
    c(
      ds_coalesce[!id_truedup], 
      list(FUN = c, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    ) |>
      do.call(what = mapply, args = _) |> # # ?base::.mapply drop names!
      as.data.frame.list(check.names = FALSE)
  } # else NULL
  
  if (any(id_truedup)) {
    n_truedup <- sum(id_truedup)
    
    dontshow_nc <- match(dontshow, table = names(data))
    show_nc <- -dontshow_nc[!is.na(dontshow_nc)]
    if (!length(show_nc)) show_nc <- TRUE
    
    # slow with big `data`!!
    nm_dup <- rid[ns] |>
      names() |>
      sQuote() |>
      format(justify = 'left')
    tmp <- mapply(
      FUN = \(d, nm) {
        #message('\rFinding duplicated columns ', nm, appendLF = FALSE)
        not_unique_(d[show_nc])
      }, 
      d = ds[id_truedup], 
      nm = sprintf(fmt = '%s - %d of %d', nm_dup[id_truedup], seq_len(n_truedup), n_truedup), 
      SIMPLIFY = FALSE
    )
    #cat('\r')
    
    write_xlsx(x = tmp, path = file)
    
    sprintf(
      fmt = '\u261e %s with substantial duplicates {.href [%s](file://{path.expand(path = file)})}', # https://cli.r-lib.org/reference/links.html
      sprintf(fmt = '%d %s', n_truedup, dup_txt) |> bg_br_yellow(),
      file |> basename() |> col_yellow()
    ) |> 
      cli_text()
    file |>
      dirname() |>
      sprintf(fmt = 'open %s') |> 
      system()
    
    if (missing(rule)) {
      '\u2765 Na\u00efvely select 1st row' |> message()
      r1_truedup <- rid[ns][id_truedup] |>
        vapply(FUN = `[`, i = 1L, FUN.VALUE = NA_integer_)
    } else {
      rule <- substitute(rule)
      if (rule[[1L]] == '{') {
        # complicated `rule` ..
        message('\u2765 Selection rule')
        print.default(rule) # not sure how to use \pkg{cli} here..
      } else {
        rule |> 
          deparse1() |>
          col_blue() |> style_bold() |>
          sprintf(fmt = '\u2765 Selection rule %s (then na\u00efvely select 1st row)') |>
          message()
      }
      id_select <- rid[ns][id_truedup] |>
        vapply(FUN = \(i) { # (i = rid[ns][id_truedup][[1L]])
          z <- rule |> 
            eval(envir = data[i, , drop = FALSE]) # inside ?base::with.default
          if (is.logical(z) && (length(z) == length(i))) {
            return(which(z)[1L]) # drops `NA`; naively select 1st row
          }
          if (is.integer(z) && length(z) == 1L) return(z)
          print(z)
          stop('illegal `z`')
        }, FUN.VALUE = NA_integer_)
      r1_truedup <- mapply(FUN = `[`, x = rid[ns][id_truedup], i = id_select, SIMPLIFY = TRUE)
    }
    
  } else r1_truedup <- NULL
  
  ret <- rbind.data.frame(
    data[c(
      rid[!ns] |> # rows of `data` without duplication
        unlist(use.names = FALSE) |>
        sort.int(),
      r1_truedup
    ), , drop = FALSE], 
    d_coalesce
  )
  sprintf(fmt = '%d %s', nrow(ret), dup_txt) |>
    style_underline() |> style_bold() |>
    sprintf(fmt = '\u21ac %s after duplicates removed') |> 
    message()
  return(ret)
  
}



unique_ <- \(x) {
  x0 <- x[!is.na(x)]
  if (!length(x0)) return(NA)
  # not using my [unique_allequal]
  if (length(x1 <- unique(x0)) != 1L) stop('non-unique entries!')
  return(x1)
}



not_unique_ <- \(data) {
  unique_id <- vapply(data, FUN = \(x) {
    #inherits(tryCatch(unique_(x), error = identity), what = 'error')
    # base::tryCatch too slow
    x0 <- x[!is.na(x)]
    if (!length(x0)) return(TRUE)
    x1 <- unique(x0) # not using my [unique_allequal]
    return(length(x1) == 1L)
  }, FUN.VALUE = NA)
  data[!unique_id]
}


