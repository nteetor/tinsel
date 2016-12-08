is.node <- function(x) inherits(x, 'node')

node <- function(token) {
  self <- new.env(parent = emptyenv())

  self$token <- token
  self$children <- stack()

  self$add <- function(object) {
    if (is.token(object)) {
      self$children$push(node(object))
    } else if (is.node(object)) {
      self$children$push(object)
    } else {
      stop('cannot add ', class(object), ' as child', call. = FALSE)
    }
    invisible(self)
  }

  class(self) <- 'node'
  self
}
