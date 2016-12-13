is.stack <- function(x) inherits(x, 'stack')

length.stack <- function(x) {
  x$size()
}

head.stack <- function(x, n = 6L, ...) {
  n <- min(length(x), n)
  stack(rev(as.list(x)[1:n]))
}

tail.stack <- function(x, n = 6L, ...) {
  n <- max(length(x) - n, 0)
  stack(rev(as.list(x)[n:length(x)]))
}

as.list.stack <- function(x, ...) {
  tryCatch(
    unname(mget(as.character(rev(seq_len(x$size()))), envir = x$values)),
    error = function(e) {
      stop('unexpected error coercing stack to list', call. = FALSE)
    }
  )
}

print.stack <- function(x, ..., n = 10) {
  smry <- sprintf('# A stack: %d', x$size())
  if (x$size() == 0) {
    cat(smry)
    return(invisible(x))
  }
  elements <- as.list(x)[1:min(n, x$size())]
  vals <- vapply(elements, function(e) as.character(e), character(1))
  clsss <- vapply(elements, function(e) class(e), character(1))
  wdths <-
    vapply(vals, nchar, numeric(1)) +
    vapply(seq_along(elements), nchar, numeric(1))
  mxwdth <- max(max(wdths), 4) + nchar(n)
  bdy <- paste0(
    vapply(
      seq_along(vals),
      function(i) {
        frmt <- paste0('%d%', mxwdth - nchar(i), 's <%s>')
        sprintf(frmt, i, vals[i], clsss[i])
      },
      character(1)
    ),
    collapse = '\n'
  )
  if (x$size() > n) {
    bdy <- paste0(bdy, paste('\n# ... with', x$size() - n, 'more items'))
  }
  cat(smry, '\n')
  cat(bdy, '\n')
  return(invisible(x))
}

as.character.stack <- function(x, ...) {
  if (x$size() == 0) return('[ ]')
  paste(
    '[',
    paste('{', vapply(as.list(x), as.character, character(1)), '}',
          collapse = ', ', sep = ''),
    ']'
  )
}

stack <- function(list = NULL) {
  self <- new.env(parent = emptyenv())

  if (is.null(list)) {
    self$values <- new.env(parent = emptyenv())
    self$cursor <- 0
  } else {
    list <- as.list(list)
    self$cursor <- length(list)
    self$values <- list2env(
      set_names(list, seq_along(list)),
      parent = emptyenv()
    )
  }

  self$push <- function(item) {
    self$cursor <- self$cursor + 1
    assign(as.character(self$cursor), item, envir = self$values)
    invisible(self)
  }
  self$pop <- function() {
    if (self$empty()) return(NULL)
    v <- get(as.character(self$cursor), envir = self$values, inherits = FALSE)
    self$cursor <- self$cursor - 1
    v
  }
  self$size <- function() {
    self$cursor
  }
  self$peek <- function() {
    if (self$empty()) return(NULL)
    get(as.character(self$cursor), envir = self$values, inherits = FALSE)
  }
  self$empty <- function() {
    self$size() == 0
  }

  class(self) <- 'stack'
  self
}

