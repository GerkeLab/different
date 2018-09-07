#' Pair x and y tables for differencing
#'
#' @param x `<tbl|matrix>` A data set (`tibble`, `data.frame`, `matrix`) or a
#'   a (named) list containing 2 data sets.
#' @param y `<tbl|matrix>` A data set (`tibble`, `data.frame`, `matrix`) with
#'   the same class as `x`. `NULL` if `x` is a list.
#' @param df_names `<chr>` Names of the two data sets, either ordered or as a named
#'   vector with names `"x"` and `"y"`
#' @param keys `<chr>` A character vector listing key columns that link rows in
#'   each data set to a row in the other.
#' @export
diff_pair <- function(x, ...) UseMethod("diff_pair")

#' @rdname diff_pair
#' @export
diff_pair.data.frame <- function(x, y = NULL, df_names = NULL, keys = NULL, ...) {
  if (is.null(y)) rlang::abort("`diff_pair()` requires two data sets.")
  diff_pair_validate_input(x, y)
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  df_class <- purrr::map(list(x = x, y = y), class)
  x <- coerce_tibble(x)
  y <- coerce_tibble(y)
  new_diff_pair(x, y, df_names, keys, df_class = df_class, ...)
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
  diff_pair(x[[1]], x[[2]], df_names, keys, ...)
}

#' @rdname diff_pair
#' @export
diff_pair.matrix <- function(x, y = NULL, df_names = NULL, keys = NULL, ...) {
  if (is.null(y)) rlang::abort("`diff_pair()` requires two data sets.")
  diff_pair_validate_input(x, y)
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  df_class <- purrr::map(list(x = x, y = y), class)
  x <- coerce_tibble(x)
  y <- coerce_tibble(y)
  new_diff_pair(x, y, df_names, keys, df_class = df_class, ...)
}

#' @export
diff_pair.default <- function(x, ...) {
  diff_pair_validate_input(x)
}

diff_pair_validate_input <- function(x, y = NULL) {
  valid <- c("tbl_df", "data.frame", "matrix")
  inputs <- list(x = x, y = y) %>% purrr::compact()
  good <- inputs %>% purrr::map_lgl(inherits, what = valid)
  if (all(good)) return(invisible(TRUE))
  verb <- if (length(inputs) == 2) "compare" else "work with"
  if (length(inputs) == 1) inputs <- inputs[!good]
  classes <- purrr::map_chr(inputs, ~ paste(class(.), collapse = "|"))
  classes <- paste(classes, paste0("(", names(classes), ")"), collapse = " to ")
  rlang::abort(glue("`diff_pair()` doesn't know how to {verb} {classes} objects"))
}

new_diff_pair <- function(x, y, df_names = NULL, keys = NULL,
                          df_class = list(x = class(x), y = class(y)), ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  structure(
    list(
      x = x,
      y = y,
      diff_meta = new_metadata(x, y, df_names, df_class = df_class, ...),
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


# ---- Helper functions to coerce inputs to tibbles ----
coerce_tibble <- function(x) {
  if (inherits(x, "tbl_df")) {
    if (!tibble::has_rownames(x)) return(x)
  }
  if (is.null(colnames(x))) x <- tibble::repair_names(x)
  row_names <- row.names(x)
  x <- as_tibble(x)
  if (!is.null(row_names)) {
    vars <- names(x)
    x$`.row.names` <- row_names
    x <- x[, c(".row.names", vars)]
  }
  x
}
