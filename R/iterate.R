itr <- function(file) {
  FileIterator$new(file)
}

FileIterator <- R6::R6Class(
  classname = 'itr',
  private = list(
    contents = NULL,
    cursor = NULL
  ),
  public = list(
    FILE_NAME = NULL,
    initialize = function(file) {
      if (!file.exists(file)) {
        stop('could not find ', file, call. = FALSE)
      }
      self$FILE_NAME <- basename(file)
      private$contents <- readLines(file)
      private$cursor <- 1
      self
    },
    num_lines = function() {
      length(private$contents)
    },
    all_lines = function() {
      private$contents
    },
    get_line = function() {
      if (private$cursor > length(private$contents)) {
        stop('at end of file', call. = FALSE)
      }
      line <- private$contents[private$cursor]
      private$cursor <- private$cursor + 1
      line
    },
    prev_line = function() {
      if (private$cursor <= 1) {
        stop('at head of file', call. = FALSE)
      }
      private$contents[private$cursor - 1]
    },
    has_next = function() {
      private$cursor <= length(private$contents)
    }
  )
)
