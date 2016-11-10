char_at <- function(.s, i) {
  if (abs(i) > nchar(.s) || i == 0) stop('index out of bounds', call. = FALSE)
  if (i > 0) {
    substr(.s, i, i)
  } else {
    j <- nchar(.s) + 1 + i
    substr(.s, j, j)
  }
}

first_of <- function(.s, c) {
  i <- 1
  if (!grepl(c, .s, fixed = TRUE)) return(-1)
  while (i <= nchar(.s) && char_at(.s, i) != c) i <- i + 1
  if (i > nchar(.s)) -1 else i
}
