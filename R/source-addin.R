source_decorated_addin <- function() {
  active <- rstudioapi::getActiveDocumentContext()
  path <- paste0('"', active$path, '"')
  library(tinsel, quietly = TRUE)
  rstudioapi::sendToConsole(paste0('source_decorated(', path, ')'))
}
