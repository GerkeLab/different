#' Pair x and y tables for differencing
#'
#' @export
diff_pair <- function(x, y, df_names = NULL, keys = NULL, ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  new_diff_pair(x, y, df_names, keys, ...)
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
print.diff_pair <- function(z) {
  cat_glue(subtle("<diff_pair: {paste(metadata(z, 'names'), collapse = ' vs ')}>"))
  print(z[c("x", "y")])
}


#' @method metadata diff_pair
metadata.diff_pair <- function(z, prop = NULL) {
  meta <- z$diff_meta
  if (!is.null(prop)) {
    prop <- match.arg(prop, names(meta))
    meta[[prop]]
  } else meta
}
