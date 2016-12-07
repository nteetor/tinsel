is.token <- function(x) inherits(x, 'token')

type <- function(tokens) {
  if (is.token(tokens)) {
    tokens$type
  } else if (is.list(tokens)) {
    vapply(tokens, `[[`, numeric(1), 'type')
  } else {
    stop('unexpected class ', class(tokens), call. = FALSE)
  }
}

`type<-` <- function(token, value) {
  if (is.null(value)) {
    stop('type cannot be NULL', call. = FALSE)
  } else if (!(value %in% .type)) {
    stop('unknown type ', value, call. = FALSE)
  }
  token$type <- value
  invisible(token)
}

value <- function(tokens) {
  if (is.token(tokens)) {
    tokens$value
  } else if (is.list(tokens)) {
    vapply(tokens, `[[`, character(1), 'value')
  } else {
    stop('unexpected class ', class(tokens), call. = FALSE)
  }
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

token <- function(value, type, lineno) {
  structure(
    list(
      type = type,
      value = value,
      lineno = lineno
    ),
    class = 'token'
  )
}
