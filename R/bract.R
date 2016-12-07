is.bract <- function(x) inherits(x, 'bract')

bract <- function(token) {
  self <- new.env(parent = emptyenv())

  self$token <- token
  self$children <- stack()

  self$add <- function(object) {
    if (is.token(object)) {
      self$children$push(bract(object))
    } else if (is.bract(object)) {
      self$children$push(object)
    } else {
      stop('bract cannot handle object of class ', class(object), call. = FALSE)
    }
  }

  class(self) <- 'bract'
  self
}
