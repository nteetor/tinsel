is.token <- function(x) inherits(x, 'token')

as.character.token <- function(x, ...) {
  lbl <- if (x$type %in% .type) names(which(.type == x$type)) else x$type
  val <- trunk(as.character(x$value), 10)
  sprintf('("%s" %s)', val, lbl)
}

print.token <- function(x, ...) {
  tom(as.character(x))
}

field <- function(tokens, f, fun_value) {
  if (is.token(tokens)) {
    tokens[[f]]
  } else if (is.list(tokens)) {
    if (length(tokens) == 0) {
      NULL
    } else if (length(tokens) == 1) {
      tokens[[1]][[f]]
    } else {
      vapply(tokens, `[[`, FUN.VALUE = fun_value, f)
    }
  } else {
    stop('unexpected class ', class(tokens), call. = FALSE)
  }
}

`field<-` <- function(tokens, f, value) {
  if (is.null(value)) {
    stop('token ', f, ' may not be NULL', call. = FALSE)
  }

  if (is.token(tokens)) {
    tokens[[f]] <- value
  } else {
    stop('unexpected class ', class(tokens), call. = FALSE)
  }

  invisible(tokens)
}

type <- function(t) field(t, 'type', numeric(1))
`type<-` <- function(t, value) `field<-`(t, 'type', value)

value <- function(t) field(t, 'value', character(1))
`value<-` <- function(t, value) `field<-`(t, 'value', value)

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
