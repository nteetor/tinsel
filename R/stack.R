is.stack <- function(x) inherits(x, 'stack')

length.stack <- function(x) {
  x$size()
}

head.stack <- function(x, n = 6L, ...) {
  n <- min(length(x), n)
  stack(rev(as.list(x)[1:n]))
}

tail.stack <- function(x, n = 6L, ...) {
  n <- max(length(x) - n, 0)
  stack(rev(as.list(x)[n:length(x)]))
}

as.list.stack <- function(x, ...) {
  tryCatch(
    unname(mget(as.character(rev(seq_len(x$size()))), envir = x$values)),
    error = function(e) {
      stop('unexpected error coercing stack to list', call. = FALSE)
    }
  )
}

summary.stack <- function(x, ..., width = 30) {
  if (length(x) == 0) return(list())
  lapply(as.list(x),
         function(s) {
           if (is.atomic(s)) {
             s
           } else {
             if (length(summary(s)) == 1) {
               summary(s, width = width)
             } else {
               s
             }
           }
         })
}

format.stack <- function(x, ..., n = 10, width = 30) {
  smry <- sprintf('# A stack: %d', length(x))

  if (length(x) == 0) return(smry)

  selected <- as.list(head(x, n))
  indeces <- seq_along(selected)
  summaries <- vapply(
    summary(head(x, n), width = width),
    function(s) if (is.atomic(s)) as.character(s) else '..',
    character(1)
  )
  classes <- vapply(selected, class, character(1))
  pformat <- paste0('%-', max(nchar(indeces)) + 1,'s',
                    '%', max(nchar(summaries)), 's',
                    ' <%s>')

  bdy <- paste0(sprintf(pformat, indeces, summaries, classes), collapse = '\n')

  output <- paste(smry, bdy, sep = '\n')
  if (n < length(x)) {
    d <- length(x) - n
    footer <- paste('# ... with', d, 'more', if (d == 1) 'item' else 'items')
    output <- paste(output, footer, sep = '\n')
  }
  output
}

print.stack <- function(x, ..., n = 10, width = 30) {
  cat(format(x, n = n, width = width))
  invisible(x)
}

stack <- function(list = NULL) {
  self <- new.env(parent = emptyenv())

  if (is.null(list)) {
    self$values <- new.env(parent = emptyenv())
    self$cursor <- 0
  } else {
    list <- as.list(list)
    self$cursor <- length(list)
    self$values <- list2env(
      set_names(list, seq_along(list)),
      parent = emptyenv()
    )
  }

  self$push <- function(item) {
    self$cursor <- self$cursor + 1
    assign(as.character(self$cursor), item, envir = self$values)
    invisible(self)
  }
  self$pop <- function() {
    if (self$empty()) return(NULL)
    v <- get(as.character(self$cursor), envir = self$values, inherits = FALSE)
    self$cursor <- self$cursor - 1
    v
  }
  self$size <- function() {
    self$cursor
  }
  self$peek <- function() {
    if (self$empty()) return(NULL)
    get(as.character(self$cursor), envir = self$values, inherits = FALSE)
  }
  self$empty <- function() {
    self$size() == 0
  }

  class(self) <- 'stack'
  self
}

