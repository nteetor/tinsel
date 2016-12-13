is.scanner <- function(x) inherits(x, 'scanner')

scanner <- function(file) {
  self <- new.env(parent = emptyenv())
  self$tokens <- stack()
  self$stream <- traversal(file)

  self$expect <- function(symbol) {
    c <- self$stream$getchar()
    if (c != symbol) {
      stop(expected(symbol, c, self$stream$lineno()))
    } else {
      c
    }
  }

  self$comment <- function() {
    c1 <- self$expect(.sym$COMMENT)
    if (self$stream$peek() == .sym$PERIOD) {
      # tinsel comment
      c2 <- self$expect(.sym$PERIOD)
      self$tokens$push(token(paste0(c1, c2), .type$TINSEL_COMMENT,
                             self$stream$lineno()))
      self$stream$skipws()
      self$filename()
    } else {
      self$stream$getline()
    }
  }
  self$filename <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$FILENAME_CHAR)) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    self$tokens$push(token(buffer, .type$FILE_REFERENCE, self$stream$lineno()))
  }
  self$identifier <- function() {
    if (self$stream$peek() == .sym$BACKTICK) {
      self$nonsyntactic()
    } else {
      self$syntactic()
    }
  }
  self$nonsyntactic <- function() {
    buffer <- self$expect(.sym$BACKTICK)
    while (self$stream$peek() != .sym$BACKTICK) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    buffer <- paste0(buffer, self$expect(.sym$BACKTICK))
    self$tokens$push(token(buffer, .type$IDENTIFIER, self$stream$lineno()))
  }
  self$syntactic <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$SYNTACTIC_CHAR)) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    if (buffer == 'function') {
      self$tokens$push(token(buffer, .type$FUNCTION_STMT, self$stream$lineno()))
    } else if (buffer %in% .reserved) {
      self$tokens$push(token(buffer, .type$RESERVED, self$stream$lineno()))
    } else {
      self$tokens$push(token(buffer, .type$IDENTIFIER, self$stream$lineno()))
    }
  }
  self$quotation <- function() {
    quotype <- self$stream$getchar()
    buffer <- quotype
    while (self$stream$peek() != quotype) {
      c <- self$stream$getchar()
      if (c == .sym$BACKSLASH && self$stream$peek() == quotype) {
        buffer <- paste0(buffer, c, self$stream$getchar())
      } else {
        buffer <- paste0(buffer, c)
      }
    }
    buffer <- paste0(buffer, self$expect(quotype))
    self$tokens$push(token(buffer, .type$STRING, self$stream$lineno()))
  }
  self$number <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$NUMBER)) {
      if (self$stream$peek() == .sym$PERIOD &&
          first_of(buffer, .sym$PERIOD) != -1) {
        self$expect(.sym$NUMBER)
      } else if (self$stream$peek() == .sym$EXPNOTATION &&
          first_of(buffer, .sym$EXPNOTATION) != -1) {
        self$expect(.sym$NUMBER)
      } else {
        buffer <- paste0(buffer, self$stream$getchar())
      }
    }
    self$tokens$push(token(buffer, .type$NUMBER, self$stream$lineno()))
  }
  self$assignment <- function() {
    buffer <- self$stream$getchar()
    if (buffer == .sym$LESSTHAN) {
      if (self$stream$peek() == .sym$MINUS) {
        buffer <- paste0(buffer, self$expect(.sym$MINUS))
        self$tokens$push(token(buffer, .type$ASSIGNOP, self$stream$lineno()))
      } else {
        self$tokens$push(token(c, .type$RELATIONALOP, self$stream$lineno()))
      }
    } else if (buffer == .sym$EQUALS) {
      self$tokens$push(token(buffer, .type$ASSIGNOP, self$stream$lineno()))
    }
  }
  self$expression <- function() {
    lineno <- self$stream$lineno()
    buffer <- self$expect(.sym$RBRACE)
    braces <- 1
    while (braces != 0) {
      c <- self$stream$getchar()

      if (c == .sym$EOF) {
        stop('reached end of file while parsing expression starting on line ',
             self$stream$lineno(), call. = FALSE)
      } else if (c == .sym$LBRACE) {
        braces <- braces + 1
      } else if (c == .sym$RBRACE) {
        braces <- braces - 1
      }

      buffer <- paste0(buffer, c)
    }
    self$tokens$push(token(buffer, .type$EXPRESSION, lineno))
  }
  self$extract <- function() {
    buffer <- self$stream$getchar()
    if (buffer == .sym$LBRACKET) {
      if (self$stream$peek() == .sym$LBRACKET) {
        buffer <- paste0(buffer, self$stream$getchar())
      }
      self$tokens$push(token(buffer, .type$EXTRACTOP, self$stream$lineno()))
    } else if (buffer == .sym$DOLLARSIGN) {
      self$tokens$push(token(buffer, .type$EXTRACTOP, self$stream$lineno()))
    }
  }

  self$tokenize <- function() {
    self$stream$reset()
    self$tokens <- stack()

    while (self$stream$peek() != .sym$EOF) {
      c <- self$stream$peek()
      if (c == .sym$COMMENT) {
        self$comment()
      } else if (re_match(c, .sym$IDENTIFIER_CHAR)) {
        self$identifier()
      } else if (c == .sym$SINGLEQUOTE || c == .sym$DOUBLEQUOTE) {
        self$quotation()
      } else if (re_match(c, .sym$NUMBER)) {
        self$number()
      } else if (c == .sym$EQUALS || c == .sym$LESSTHAN) {
        self$assignment()
      } else if (c == .sym$DOLLARSIGN || c == .sym$LBRACKET) {
        self$extract()
      } else {
        self$stream$getchar()
      }
    }
    self$tokens$push(token(.sym$EOF, .type$EOF, self$stream$lineno()))

    self$tokens
  }

  class(self) <- 'scanner'
  self
}
