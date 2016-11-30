traverse <- function(file) {
  fsize <- (file.info(file))$size
  ffile <- file(file, open = 'r')

  self <- new.env(parent = baseenv())
  self$chars <- strsplit(readChar(ffile, nchars = fsize), '')[[1]]
  close(ffile)

  self$cursor <- 1
  self$EOF <- .sym$EOF
  self$EOL <- .sym$EOL

  self$at_eof <- function() {
    self$cursor > length(self$chars)
  }
  self$size <- function() {
    length(self$chars)
  }
  self$increment_cursor <- function() {
    self$cursor <- min(self$cursor + 1, self$size() + 1)
  }
  self$decrement_cursor <- function() {
    self$cursor <- max(self$cursor - 1, 1)
  }

  self$getchar <- function() {
    if (self$at_eof()) return(self$EOF)

    c <- self$chars[self$cursor]
    self$increment_cursor()
    c
  }
  self$getregex <- function(regex) {
    buffer <- NULL
    while (re_match((c <- self$getchar()), regex) && c != self$EOF) {
      buffer <- paste0(buffer, c)
    }
    self$decrement_cursor()
    buffer
  }
  self$getline <- function() {
    buffer <- NULL
    while ((c <- self$getchar()) != self$EOL && c != self$EOF) {
      buffer <- c(buffer, c)
    }
    buffer
  }

  self$putchar <- function(c) {
    self$decrement_cursor()
    if (c != self$chars[self$cursor]) {
      self$chars[(self$cursor:self$size()) + 1] <- self$chars[self$cursor:self$size()]
      self$chars[self$cursor] <- c
    }
  }

  self$peek <- function() {
    self$chars[self$cursor]
  }
  self$lookfor <- function(s) {
    eolindex <- which(self$chars[self$cursor:self$size()] == self$EOL)[1]
    toeol <- paste0(self$chars[self$cursor:eolindex], collapse = '')
    is.null(re_search(toeol, s))
  }

  self$skipws <- function() {
    while (re_match(self$getchar(), '\\h')) {}
    self$decrement_cursor()
  }
  self$expect <- function(symbol) {
    if ((c <- self$getchar()) != symbol) {
      lineno <- sum(self$chars[1:self$cursor] == self$EOL, na.rm = TRUE)
      stop(expected(symbol, c, lineno))
    }
  }

  self
}
