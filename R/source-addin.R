source_decoratees_addin <- function() {
  active <- rstudioapi::getActiveDocumentContext()
  path <- paste0('"', active$path, '"')
  rstudioapi::sendToConsole(paste0('source_decoratees(', path, ')'))
}
