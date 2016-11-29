scanner <- function(file) {
  self <- new.env(parent = emptyenv())
  self$tokens <- stack()
  self$stream <- traverse(file)

  self$digest_decorator <- function() {
    # tinsel comment
    self$stream$skipws()

    buffer <- NULL
    while ((c <- self$stream$getchar()) != .sym$EOL) {
      if (c == .sym$DOLLARSIGN) {
        self$tokens$push(token(buffer, .type$FILE_REFERENCE))
        self$tokens$push(token(.sym$DOLLARSIGN, .type$FILE_INDEXING))
        buffer <- NULL
      }

      buffer <- paste0(buffer, c)
    }

    self$tokens$push(token(buffer, .type$DECORATOR))
  }
  self$digest_decoratee <- function() {
    self$stream$skipws()

    buffer <- NULL
    while (re_match((c <- self$stream$getchar()), .sym$SYNTACTIC_CHAR)) {
      buffer <- paste0(buffer, c)
    }

    self$tokens$push(token(buffer, .type$DECORATEE))

    self$stream$skipws()
    self$stream$expect(.sym$LESSTHAN)
    self$stream$expect(.sym$MINUS)
  }

  self$tokenize <- function() {
    while ((c <- self$stream$getchar()) != .sym$EOF) {

      if (c == .sym$COMMENT) {
        if ((c <- self$stream$getchar()) == .sym$PERIOD) {
          self$digest_decorator()
#          self$digest_decoratee(), need to check for additional decorators
        }
      }
    }

    self$tokens$tolist()
  }

  self
}
