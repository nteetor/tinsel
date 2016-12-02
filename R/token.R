is.token <- function(x) inherits(x, 'token')

token <- function(value, type) {
  structure(
    list(
      type = type,
      value = value
    ),
    class = 'token'
  )
}
