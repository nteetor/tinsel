file_itr <- function(file) {
  if (!file.exists(file)) {
    stop('could not find file ', file, call. = FALSE)
  }
  self <- new.env(parent = baseenv())
  self$contents <- readLines(file)
  self$cursor <- 1
  self$FILE_NAME <- basename(file)

  self$get_line <- function() {
    if (self$cursor > length(self$contents)) {
      stop('at end of file', call. = FALSE)
    }
    line <- self$contents[self$cursor]
    self$cursor <- self$cursor + 1
    line
  }
  self$has_next <- function() {
    self$cursor <= length(self$contents)
  }

  class(self) <- c('file_itr', class(self))

  self
}
