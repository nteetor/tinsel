char_at <- function(.string, i) {
  if (abs(i) > nchar(.string) || i == 0) stop('index out of bounds', call. = FALSE)
  j <- (i %% nchar(.string)) + (i < 0)
  substr(.string, j, j)
}

first_of <- function(.string, c) {
  if (!grepl(c, .string, fixed = TRUE)) return(-1)
  i <- 1
  while (char_at(.string, i) != c) i <- i + 1
  i
}

pairstring <- function(f) {
  x <- vapply(formals(f), as.character, character(1))
  x <- paste(names(x), x, sep = '=')
  x <- gsub('=$', '', x)
  paste(x, collapse = ', ', sep = '')
}
