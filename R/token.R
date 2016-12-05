is.token <- function(x) inherits(x, 'token')

type <- function(t) {
  if (!is.token(t)) stop('unexpected class ', class(t), call. = FALSE)
  t$type
}

value <- function(t) {
  if (!is.token(t)) stop('unexpected class ', class(t), call. = FALSE)
  t$value
}

token <- function(value, type) {
  structure(
    list(
      type = type,
      value = value
    ),
    class = 'token'
  )
}
