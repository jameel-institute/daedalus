#' Helper functions
#' 
#' @name tools
#' @rdname tools
#'
#' @keywords internal
drop_null <- function(x) {
  Filter(function(y) !is.null(y), x)
}
