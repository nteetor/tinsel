is.stack <- function(x) inherits(x, 'stack')

stack <- function(list = NULL) {
  self <- new.env(parent = emptyenv())

  if (is.null(list)) {
    self$values <- new.env(parent = emptyenv())
    self$cursor <- 1
  } else {
    self$cursor <- length(list) + 1
    self$values <- list2env(
      set_names(list, seq_along(list)),
      parent = emptyenv()
    )
  }

  self$push <- function(item) {
    assign(as.character(self$cursor), item, envir = self$values)
    self$cursor <- self$cursor + 1
  }
  self$pop <- function() {
    if (self$cursor - 1 <= 0) return(NULL)
    self$cursor <- self$cursor - 1
    get(as.character(self$cursor), envir = self$values, inherits = FALSE)
  }
  self$size <- function() {
    self$cursor - 1
  }
  self$tolist <- function() {
    set_names(
      mget(as.character(seq_len(self$size())), self$values, inherits = FALSE),
      NULL
    )
  }

  class(self) <- 'stack'
  self
}

