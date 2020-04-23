#' Attach unexported items from namespace
#' @description Purely used for development
#'
#' @export

attachme <- function() {
  invisible(eval.parent(attach(asNamespace(packageName()))))
}
