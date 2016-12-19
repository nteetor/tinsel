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
    c <- self$expect(.sym$COMMENT)
    if (self$stream$peek() == .sym$PERIOD) {
      self$tokens$push(token(paste0(c, self$expect(.sym$PERIOD)),
                             .type$TINSEL_COMMENT,
                             self$stream$lineno()))
      self$filename()
      self$extract()
      self$identifier()
      self$arguments()

      self$stream$skipws()
      self$expect(.sym$EOL)
    } else {
      self$stream$getline()
    }
  }
  self$filename <- function() {
    self$stream$skipws()

    if (re_match(self$stream$peek(), .sym$FILENAME_CHAR)) {
      buffer <- NULL

      while (re_match(self$stream$peek(), .sym$FILENAME_CHAR)) {
        buffer <- paste0(buffer, self$stream$getchar())
      }

      self$tokens$push(token(buffer, .type$IDENTIFIER, self$stream$lineno()))
    }
  }
  self$extract <- function() {
    self$stream$skipws()

    if (self$stream$peek() == .sym$DOLLARSIGN) {
      self$tokens$push(token(self$expect(.sym$DOLLARSIGN),
                             .type$EXTRACT_OPERATOR,
                             self$stream$lineno()))
    }
  }
  self$identifier <- function() {
    self$stream$skipws()

    if (self$stream$peek() == .sym$BACKTICK) {
      self$nonsyntactic()
    } else if (re_match(self$stream$peek(), .sym$SYNTACTIC_CHAR)) {
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
      self$tokens$push(token(buffer, .type$FUNCTION_OPERATOR,
                             self$stream$lineno()))
    } else if (buffer %in% .reserved) {
      self$tokens$push(token(buffer, .type$REXPRESSION, self$stream$lineno()))
    } else {
      self$tokens$push(token(buffer, .type$IDENTIFIER, self$stream$lineno()))
    }
  }
  self$arguments <- function() {
    self$stream$skipws()

    if (self$stream$peek() == .sym$LPAREN) {
      self$tokens$push(token(self$expect(.sym$LPAREN), .type$LPAREN,
                             self$stream$lineno()))

      while (self$stream$peek() != .sym$RPAREN) {
        self$argument()
      }

      self$stream$push(token(self$expect(.sym$RPAREN), .type$RPAREN,
                             self$stream$lineno()))
    }
  }
  self$argument <- function() {
    self$stream$skipws()

    buffer <- ''
    while ((p <- self$stream$peek()) != .sym$COMMA &&
           p != .sym$RPAREN) {
      if (p %in% c(.sym$LPAREN, .sym$LBRACKET, .sym$LBRACE)) {
        buffer <- paste0(buffer, self$brackets())
      } else if (p == .sym$DOUBLEQUOTE || p == .sym$SINGLEQUOTE) {
        buffer <- paste0(buffer, self$quotation())
      } else if (re_match(p, .sym$WHITESPACE)) {
        self$stream$getchar()
      } else if (p == .sym$EQUALS) {
        if (buffer == '') {
          stop(expected(.sym$SYNTACTIC_CHAR, .sym$EQUALS, self$stream$lineno()))
        }
        self$tokens$push(token(buffer, .type$IDENTIFIER, self$stream$lineno()))
        self$tokens$push(token(self$expect(.sym$EQUALS)), .type$ASSIGN_OPERATOR,
                         self$stream$lineno())
        buffer <- ''
      } else if (p == .sym$BACKTICK) {
        self$identifier()
      } else {
        buffer <- paste0(buffer, self$stream$getchar())
      }
    }

    # consume a comma
    if (self$stream$peek() == .sym$COMMA) {
      self$stream$getchar()
    }

    self$tokens$push(token(buffer, .type$REXPRESSION, self$stream$lineno()))
  }
  # returns a value unlike other methods which push tokens onto the stack
  self$brackets <- function() {
    buffer <- self$stream$getchar()

    bsyms <- NULL
    if (buffer == .sym$LPAREN) {
      bsyms <- c(.sym$LPAREN, .sym$RPAREN)
    } else if (buffer == .sym$LBRACE) {
      bsyms <- c(.sym$LBRACE, .sym$RBRACE)
    } else if (buffer == .sym$LBRACKET) {
      bsyms <- c(.sym$LBRACKET, .sym$RBRACKET)
    } else {
      stop(expected('A BRACKET', buffer, self$stream$lineno()))
    }

    depth <- 1
    while (depth) {
      p <- self$stream$getchar()

      if (p == bsyms[1]) {
        depth <- depth + 1
      } else if (p == bsyms[2]) {
        depth <- depth - 1
      } else if (p == .sym$EOF) {
        stop('reached EOF while parsing ', bsyms[1], ' on line ',
             self$stream$lineno(), call. = FALSE)
      }

      buffer <- paste0(buffer, p)
    }

    buffer
  }
  # like `argument()` this method returns a value rather than push tokens
  self$quotation <- function() {
    quotype <- self$stream$getchar()
    buffer <- quotype
    while (self$stream$peek() != quotype) {
      c <- self$stream$getchar()
      if (c == .sym$BACKSLASH && self$stream$peek() == quotype) {
        buffer <- paste0(buffer, c, self$stream$getchar())
      } else if (c == .sym$EOF) {
        stop('reached EOF while parsing string on line ', self$stream$lineno(),
             call. = FALSE)
      } else {
        buffer <- paste0(buffer, c)
      }
    }
    buffer <- paste0(buffer, self$expect(quotype))
    buffer
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
    self$tokens$push(token(buffer, .type$REXPRESSION, self$stream$lineno()))
  }
  self$assignment <- function() {
    buffer <- self$stream$getchar()
    if (buffer == .sym$LESSTHAN) {
      if (self$stream$peek() == .sym$MINUS) {
        buffer <- paste0(buffer, self$expect(.sym$MINUS))
        self$tokens$push(token(buffer, .type$ASSIGN_OPERATOR,
                               self$stream$lineno()))
      } else {
        self$tokens$push(token(c, .type$REXPRESSION, self$stream$lineno()))
      }
    } else if (buffer == .sym$EQUALS) {
      self$tokens$push(token(buffer, .type$ASSIGN_OPERATOR,
                             self$stream$lineno()))
    }
  }

  self$tokenize <- function() {
    self$stream$reset()
    self$tokens <- stack()

    buffer <- NULL
    while ((c <- self$stream$peek()) != .sym$EOF) {
      if (c == .sym$COMMENT) {
        if (!is.null(buffer)) {
          if (!re_match(buffer, '\\s+')) {
            self$tokens$push(buffer, .type$REXPRESSION, self$stream$lineno())
          }
          buffer <- NULL
        }
        self$comment()
      } else if (c == .sym$EOL) {
        self$stream$getchar()
      } else {
        buffer <- paste0(buffer, self$stream$getchar())
      }
    }
    if (!is.null(buffer)) {
      self$tokens$push(token(buffer, .type$REXPRESSION, self$stream$lineno()))
    }
    self$tokens$push(token(.sym$EOF, .type$EOF, self$stream$lineno()))

    self$tokens
  }

  class(self) <- 'scanner'
  self
}
