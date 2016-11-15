source_decoratees_addin <- function() {
  if (!requireNamespace('rstudioapi')) {
    stop('please install the rstudioapi package', call. = FALSE)
  }

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
