source_decoratees_addin <- function() {
  active <- tryCatch(
    rstudioapi::getSourceEditorContext(),
    error = function(e) {
      list(
        id = '',
        path = '',
        contents = ''
      )
    })

  if (active$path != '') {
    cmd <- paste0('source_decoratees("', active$path, '")')
    rstudioapi::sendToConsole(cmd)
  }
}
