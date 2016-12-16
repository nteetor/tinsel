is.token <- function(x) inherits(x, 'token')

as.character.token <- function(x, ...) {
  lbl <- if (x$type %in% .type) names(which(.type == x$type)) else x$type
  val <- trunk(as.character(x$contents), 10)
  sprintf('("%s" %s)', val, lbl)
}

print.token <- function(x, ...) {
  tom(as.character(x))
}

type <- function(x, ...) UseMethod('type')

type.default <- function(x, ...) {
  stop('no token type for class ', class(x), call. = FALSE)
}

type.token <- function(x, ...) {
  x$type
}

type.list <- function(x, ...) {
  if (length(x) == 0) {
    NULL
  } else if (length(x) == 1) {
    x[[1]][['type']]
  } else {
    vapply(x, type, numeric(1))
  }
}

`type<-` <- function(x, value) UseMethod('type<-')

`type<-.token` <- function(x, value) {
  if (is.null(value)) {
    stop('token type may not be NULL', call. = FALSE)
  }
  x$type <- value
  invisible(x)
}

contents <- function(x, ...) UseMethod('contents')

contents.default <- function(x, ...) {
  stop('no token contents for class ', class(x), call. = FALSE)
}

contents.token <- function(x, ...) {
  x$contents
}

contents.list <- function(x, ...) {
  if (length(x) == 0) {
    NULL
  } else if (length(x) == 1) {
    x[[1]][['contents']]
  } else {
    vapply(x, `[[`, character(1), 'contents')
  }
}

`contents<-` <- function(x, value) UseMethod('contents<-')

`contents<-.token` <- function(x, value) {
  if (!is.character(value)) {
    stop('token value must be of class character', call. = FALSE)
  }
  x$contents <- value
  invisible(x)
}

c.token <- function(t1, t2) {
  if (is.token(t2)) {
    list(t1, t2)
  } else if (is.list(t2)) {
    c(list(t1), t2)
  } else {
    stop('cannot combine token with ', class(t2), call. = FALSE)
  }
}

token <- function(contents, type, lineno) {
  structure(
    list(
      contents = contents,
      type = type,
      lineno = lineno
    ),
    class = 'token'
  )
}
