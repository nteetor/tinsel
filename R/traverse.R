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
  self$increment_cursor <- function() {
    self$cursor <- self$cursor + 1
  }
  self$decrement_cursor <- function() {
    self$cursor <- max(self$cursor - 1, 1)
  }
  self$getchar <- function() {
    if (self$at_eof()) return(self$EOF)

    c <- self$chars[[self$cursor]]
    self$increment_cursor()
    c
  }
  self$getre <- function(regex) {
    buffer <- NULL
    while (re_match((c <- self$getchar()), regex)) buffer <- paste0(buffer, c)
    self$decrement_cursor()
    buffer
  }
  self$getline <- function() {
    buffer <- NULL
    while ((c <- self$getchar()) != self$EOL) buffer <- c(buffer, c)
    buffer
  }
  self$skipws <- function() {
    while (re_match(self$getchar(), '\\h')) {}
    self$decrement_cursor()
  }
  self$expect <- function(symbol) {
    if ((c <- self$getchar()) != symbol) {
      stop(expected(symbol, c))
    }
  }

  self
}
