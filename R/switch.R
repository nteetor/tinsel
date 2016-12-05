is_blank_name <- function(x) is.name(x) && (nchar(x) == 0)

if_in <- function(lookup, actual, ..., env = parent.frame()) {
  actual <- as.character(actual)
  cases <- eval(substitute(alist(...)))
  if (!all(names(cases) %in% names(lookup))) {
    unexp <- setdiff(names(cases), names(lookup))
    missingno <- paste(unexp, collapse = ', ')
    stop('unexpected name(s) ', missingno, call. = FALSE)
  }

  accept <- FALSE

  feval <- function(x) {
    tryCatch(
      ex <- eval(x, envir = env),
      error = function(e) {
        stop('no definition found for ', deparse(x), call. = FALSE)
      })
    if (is.function(ex)) ex() else ex
  }

  for (nm in names(cases)) {
    id <- as.character(lookup[[nm]])
    is_match <- re_match(actual, id)

    if (is_blank_name(cases[[nm]])) {
      if (is_match) {
        accept <- TRUE
      }
      next
    }

    case <- cases[[nm]]

    if (accept) {
      return(feval(case))
    }

    if (is_match) {
      return(feval(case))
    }
  }

  invisible(NULL)
}

if_symbol <- function(actual, ...) {
  if_in(.sym, substitute(actual), ..., env = parent.frame())
}

if_type <- function(actual, ...) {
  if_in(.type, substitute(actual), ..., env = parent.frame())
}
