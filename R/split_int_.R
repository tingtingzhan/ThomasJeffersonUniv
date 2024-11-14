

split_int_ <- function(data, f) {
  nr <- .row_names_info(data, type = 2L)
  
  x <- all.vars(f)
  x_ <- make.names(x)
  if (length(id <- which((x != x_)))) {
    # stop('?base::.formula2varlist can only handle syntactically-valid names (i.e., ?base::make.names), as of R 4.1')
    
    nm_old <- names(data)
    tmp <- match(nm_old, table = x[id])
    names(data)[which(!is.na(tmp))] <- x_[tmp[!is.na(tmp)]]
    
    f_new <- deparse1(f)
    for (i in id) { # (i = 1L)
      f_new <- gsub(pattern = paste0('`', x[i], '`'), replacement = x_[i], x = f_new)
    } # stupid but simple :)
    f <- eval(parse(text = f_new))
  }
  
  f_ <- .formula2varlist(formula = f, data = data) # see ?base::split.data.frame
  # lapply(f_, head)
  # lengths(f_)
  # sum(Reduce(f = `&`, x = lapply(f_, FUN = function(i) !is.na(i)))) # complete `f_`
  if (anyNA(f_, recursive = TRUE)) {
    f_na <- names(which(vapply(f_, FUN = anyNA, FUN.VALUE = NA)))
    stop('missingness in ', paste(sQuote(f_na), collapse = ', '), '!\n?base::split.default ignores incomplete records')
  }
  
  rid <- split.default(x = seq_len(nr), f = f_)
  # sum(lengths(rid)) # only complete `f_` !!!
  
  return(rid[lengths(rid, use.names = FALSE) > 0L])
}