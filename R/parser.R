is.parser <- function(x) inherits(x, 'parser')

parser <- function(file) {
  self <- new.env(parent = emptyenv())
  self$scanner <- scanner(file)
  self$tokens <- stack() # stack of a stack reverses
  self$lookup <- {
    src <- new.env(parent = baseenv())
    source(file, local = src, keep.source = FALSE, verbose = FALSE)
    src
  }
  self$parsed <- stack()

  self$shift <- function() {
    self$parsed$push(node(self$tokens$pop()))
  }

  self$reduce <- function(from, to) {
    reducee <- node(token('', to, self$tokens$peek()$lineno))
    for (tipe in from) {
      if (!(tipe %in% .type)) {
        stop('on line ', reducee$token$lineno, ', undefined expected type ', noquote(tipe), call. = FALSE)
      }

      lookahead <- self$tokens$peek()
      if (is.null(lookahead)) {
        stop('on line ', reducee$token$lineno, ', exhausted stack while',
             ' reducing to type ', names(which(.type == to)), call. = FALSE)
      }
      if (type(lookahead) != tipe) {
        stop('on line ', reducee$token$lineno, ', reducing to ', to,
             ', expected type ', tipe, ', but found ', type(lookahead),
             call. = FALSE)
      }

      reducee$add(self$tokens$pop())
    }
    self$parsed$push(reducee)
  }

  self$step <- function() {
    tryCatch(
      action <- .actions[[as.character(rev(type(as.list(self$parsed))))]],
      error = function(e) {
        stop('invalid stack sequence', call. = FALSE)
      })

    if (!is.null(action[[.type$SHIFT]])) {
      self$shift()
    } else if (!is.null(action[[.type$REDUCE]])) {
      reduction <- action[[.type$REDUCE]]
      if (is.null(reduction$from) || is.null(reduction$to)) {
        stop('undefined actions for REDUCE', call. = FALSE)
      }
      if (!(reduction$from %in% .type) || !(reduction$to %in% .type)) {
        stop('unknown type value ', call. = FALSE)
      }

      self$reduce(reduction$from, reduction$to)
    } else {
      stop('no SHIFT or REDUCE action defined', call. = FALSE)
    }

  }

  self$parse <- function() {
    self$tokens <- stack(self$scanner$tokenize())

    while (length(self$tokens)) {
      self$step()
    }

    self$parsed
  }

  class(self) <- 'parser'
  self
}
