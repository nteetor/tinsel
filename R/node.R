is.node <- function(x) inherits(x, 'node')

as.character.node <- function(x, ...) {
  paste(as.character(x$token), sprintf('[%d]', length(x$children)))
}

print.node <- function(x, ...) {
  smry <- '# A node:'
  chtok <- as.character(x$token)
  hdr <- sprintf(paste0('%', nchar(chtok), 's'), paste0('<', class(x$token), '>'))
  bdy <- paste(hdr, as.character(x$token),
               paste('# ... with', x$children$size(), if (x$children$size() == 1)
                 'child' else 'children'),
               sep = '\n')
  tom(smry)
  tom(bdy)
  invisible(x)
}

descend <- function(x, ..., by = 1, every = 4) {
  indent <- list(...)[['indent']] %||% 0
  tom(dimple(x, indent = indent, every = every))
  chitlins <- rev(as.list(x$children))
  for (c in chitlins) {
    descend(c, indent = indent + by)
  }
  if (length(chitlins) && indent != 0) {
    tom(strrep('\b', every * indent), strrep(' ', every * indent))
  }
}

node <- function(token) {
  self <- new.env(parent = emptyenv())

  self$token <- token
  self$children <- stack()

  self$add <- function(object) {
    if (is.token(object)) {
      self$children$push(node(object))
    } else if (is.node(object)) {
      self$children$push(object)
    } else {
      stop('cannot add ', class(object), ' as child', call. = FALSE)
    }
    invisible(self)
  }

  class(self) <- 'node'
  self
}
