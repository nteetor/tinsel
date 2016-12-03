is.scanner <- function(x) inherits(x, 'scanner')

scanner <- function(file) {
  self <- new.env(parent = emptyenv())
  self$tokens <- stack()
  self$stream <- traversal(file)

  self$comments <- function() {
    while (self$stream$peek() == .sym$COMMENT) {
      self$comment()

      # skip over leading white space before a comment
      self$stream$skipws()
    }
  }
  self$comment <- function() {
    self$stream$expect(.sym$COMMENT)
    if (self$stream$peek() == .sym$PERIOD) {
      # tinsel comment
      self$stream$expect(.sym$PERIOD)
      self$tokens$push(token(paste0(.sym$COMMENT, .sym$PERIOD), .type$TINSEL_COMMENT))
      self$decoration()
    }

    # digest characters through end of line (including newline)
    self$stream$getline()
  }
  self$decoration <- function() {
    self$stream$skipws()
    self$dreference()
    self$stream$skipws()
    self$dcall()
  }
  self$dreference <- function() {
    if (self$stream$peek() == .sym$LBRACKET) {
      self$stream$expect(.sym$LBRACKET)
      self$filename()
      self$stream$expect(.sym$RBRACKET)
    }
  }
  self$filename <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$FILENAME_CHAR)) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    self$tokens$push(token(buffer, .type$FILE_REFERENCE))
  }
  self$dcall <- function() {
    self$identifier()
    if (self$stream$peek() == .sym$COLON) {
      self$stream$expect(.sym$COLON)
      self$stream$expect(.sym$COLON)
      self$tokens$push(token(strrep(.sym$COLON, 2), .type$PACKAGE_ACCESSOR))

      self$identifier()
    }
  }
  self$identifier <- function() {
    if (self$stream$peek() == .sym$BACKTICK) {
      self$nonsyntactic()
    } else {
      self$syntactic()
    }
  }
  self$nonsyntactic <- function() {
    self$stream$expect(.sym$BACKTICK)
    buffer <- NULL
    while (self$stream$peek() != .sym$BACKTICK) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    self$tokens$push(token(buffer, .type$IDENTIFIER))
    self$stream$expect(.sym$BACKTICK)
  }
  self$syntactic <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$SYNTACTIC_CHAR)) {
      buffer <- paste0(buffer, self$stream$getchar())
    }
    if (buffer %in% .reserved) {
      self$tokens$push(token(buffer, .type$RESERVED))
    } else {
      self$tokens$push(token(buffer, .type$IDENTIFIER))
    }
  }
  self$quotation <- function() {
    # this may seem odd, but is necessary
    if (self$stream$peek() == .sym$SINGLEQUOTE) {
      qtype <- .sym$SINGLEQUOTE
    } else {
      # here is where we catch a potential incorrect character by assuming
      # if not single quote then double quote
      qtype <- .sym$DOUBLEQUOTE
    }

    self$stream$expect(qtype)
    buffer <- NULL
    while (self$stream$peek() != qtype) {
      c <- self$stream$getchar()
      if (c == .sym$BACKSLASH && self$stream$peek() == qtype) {
        buffer <- paste0(buffer, c, self$stream$getchar())
      } else {
        buffer <- paste0(buffer, c)
      }
    }
    self$stream$expect(qtype)
    self$tokens$push(token(buffer, .type$STRING))
  }
  self$number <- function() {
    buffer <- NULL
    while (re_match(self$stream$peek(), .sym$NUMBER)) {
      if (self$stream$peek() == .sym$PERIOD &&
          first_of(buffer, .sym$PERIOD) != -1) {
        self$stream$expect(.sym$NUMBER)
      } else if (self$stream$peek() == .sym$EXPNOTATION &&
          first_of(buffer, .sym$EXPNOTATION) != -1) {
        self$stream$expect(.sym$NUMBER)
      } else {
        buffer <- paste0(buffer, self$stream$getchar())
      }
    }
    self$tokens$push(token(buffer, .type$NUMBER))
  }

  self$tokenize <- function() {
    while (self$stream$peek() != .sym$EOF) {
      c <- self$stream$peek()
      if (c == .sym$COMMENT) {
        self$comments()
      } else if (re_match(c, .sym$IDENTIFIER_CHAR)) {
        self$identifier()
      } else if (c == .sym$SINGLEQUOTE || c == .sym$DOUBLEQUOTE) {
        self$quotation()
      } else if (re_match(c, .sym$NUMBER)) {
        self$number()
      } else {
        self$stream$getchar()
      }
    }

    self$stream$reset()
    self$tokens$tolist()
  }

  class(self) <- 'scanner'
  self
}
