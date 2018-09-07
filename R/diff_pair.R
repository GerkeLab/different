#' Pair x and y tables for differencing
#'
#' @param x `<tbl|matrix>` A data set (`tibble`, `data.frame`, `matrix`) or a
#'   a (named) list containing 2 data sets.
#' @param y `<tbl|matrix>` A data set (`tibble`, `data.frame`, `matrix`) with
#'   the same class as `x`. `NULL` if `x` is a list.
#' @export
diff_pair <- function(x, y = NULL, ...) UseMethod("diff_pair")

#' @rdname diff_pair
#' @param df_names `<chr>` Names of the two data sets, either ordered or as a named
#'   vector with names `"x"` and `"y"`
#' @param keys `<chr>` A character vector listing key columns that link rows in
#'   each data set to a row in the other.
#' @export
diff_pair.data_frame <- function(x, y, df_names = NULL, keys = NULL, ...) {
  if (is.null(y)) rlang::abort("`diff_pair()` requires two data sets.")
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  new_diff_pair(x, y, df_names, keys, ...)
}

#' @rdname diff_pair
#' @export
diff_pair.list <- function(x, df_names = names(x), keys = NULL, ...) {
  x_name <- paste(sys.call())[2]
  if (length(x) < 2) rlang::abort("`diff_pair()` requires a list with two data sets.")
  if (length(x) > 2) {
    rlang::warn(glue("{x_name} contains {length(x)} elements, using only the first two."))
    x <- x[1:2]
  }
  if (is.null(df_names)) df_names <- glue("{x_name}[[{1:2}]]")
  new_diff_pair(x[[1]], x[[2]], df_names, keys, ...)
}

#' @export
diff_pair.default <- function(x, ...) {
  rlang::abort(glue("`diff_pair()` doesn't know how to work with {paste(class(x), collapse = '|')} objects"))
}

new_diff_pair <- function(x, y, df_names = NULL, keys = NULL, ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  structure(
    list(
      x = x,
      y = y,
      diff_meta = new_metadata(x, y, df_names, ...),
      action = list(
        cols_rename = list(x = NULL, y = NULL),
        cols_select = list(x = NULL, y = NULL),
        cols_keys = keys,
        vals_mutate = NULL
      )
    ),
    class = "diff_pair"
  )
}

#' @method print diff_pair
#' @export
print.diff_pair <- function(z) {
  cat_glue(subtle("<diff_pair: {paste(metadata(z, 'names'), collapse = ' vs ')}>"))
  print(z[c("x", "y")])
}


#' @method metadata diff_pair
#' @export
metadata.diff_pair <- function(z, prop = NULL) {
  meta <- z$diff_meta
  if (!is.null(prop)) {
    prop <- match.arg(prop, names(meta))
    meta[[prop]]
  } else meta
}

