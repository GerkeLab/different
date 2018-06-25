#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Null or Other Operator
#'
#' @keywords internal
#' @export
#' @usage lhs %||% rhs
#' @rdname null_or
`%||%` <- function(x, y) if (is.null(x)) y else x
