is.parser <- function(x) inherits(x, 'parser')

parser <- function(file) {
  self <- new.env(parent = emptyenv())
  self$scanner <- scanner(file)
  self$tokens <- stack() # stack of a stack reverses
  self$table <- {
    src <- new.env(parent = baseenv())
    source(file, local = src, keep.source = FALSE, verbose = FALSE)
    src
  }
  self$stack <- stack()
  self$tree <- NULL

  self$expect <- function(symbol) {
    if (type(self$tokens$peek()) != symbol) {
      stop(expected(symbol, t$value, t$lineno))
    }
    self$tokens$pop()
  }

  # start => S | `nil`
  self$parse <- function() {
    self$tokens <- stack(clone(self$scanner$tokenize()))
    self$stack <- stack()
    self$tree <- node(token('', .type$SOF, 1))

    self$S()
    self$tree
  }

  # S => A | A A
  self$S <- function() {
    while (type(self$tokens$peek()) != .type$EOF) {
      self$A()
    }
  }

  # A => #. B Z | \R
  self$A <- function() {
    t <- self$tokens$pop()
    if (type(t) == .type$TINSEL_COMMENT) {
      tinselcmt <- node(t)
      self$B()
      tinselcmt$add(self$stack$pop())
      self$Z()
      tinselcmt$add(self$stack$pop())

      self$tree$add(tinselcmt)
    }
  }

  # Z => `variable name`    // looked up in self$table
  self$Z <- function() {
    t <- self$expect(.type$IDENTIFIER)
    if (!exists(value(t), self$table, inherits = FALSE)) {
      stop('on line ', t$lineno, ', object ', value(t), ' not found',
           call. = FALSE)
    }
    decoratee <- node(token('', .type$DECORATEE, t$lineno))
    decoratee$add(t)
    self$stack$push(decoratee)
  }

  # B => [D] E | E
  self$B <- function() {
    t <- self$tokens$peek()
    self$stack$push(node(token('', .type$DECORATOR, t$lineno)))
    if (type(t) == .type$FILE_REFERENCE) {
      self$D()
      self$E()
    } else if (type(t) == .type$IDENTIFIER) {
      self$E()
    } else {
      self$expect(.type$IDENTIFIER) # not great
    }
  }

  # D => `file` | `file.extension`
  self$D <- function() {
    decorator <- self$stack$pop()
    decorator$add(self$expect(.type$FILE_REFERENCE))
    self$stack$push(decorator)
  }

  # E => G | G(H)
  self$E <- function() {
    self$G()
    #self$H()  # currently not handling arguments to decorators
  }

  # G => `package`::`decorator` | `decorator`
  self$G <- function() {
    tbd <- self$expect(.type$IDENTIFIER)
    call_nd <- node(token('', .type$CALL, tbd$lineno))

    if (type(self$tokens$peek()) == .type$PACKAGE_ACCESSOR) {
      type(tbd) <- .type$PACKAGE_NAME
      packageref <- node(tbd)
      accessor <- node(self$expect(.type$PACKAGE_ACCESSOR))
      decorator <- node(self$expect(.type$IDENTIFIER))

      accessor$add(packageref)$add(decorator)
      call_nd$add(accessor)
    } else {
      decorator <- node(tbd)
      call_nd$add(decorator)
    }

    self$stack$push(call_nd)
  }

  # H => I | `nil`   // For now I am not doing little but pass arguments
  #                  // to the decorator
  self$H <- function() {
    NULL
  }

  # I => \R | \R, I
  self$I <- function() {
    NULL
  }

  class(self) <- 'parser'
  self
}
