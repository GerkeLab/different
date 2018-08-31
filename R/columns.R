# ---- diff_cols_common ----

#' @title Common columns
#' @param x `<diff_tbl|tbl>` `diff_tbl` output from [diff_compare()] or the
#'   reference data frame or tibble.
#' @param .y `<tbl>` Comparison data frame or tibble
#' @param df_names `<chr>` Vector of length two containg the names of the data
#'   frames that should be displayed in the report
#' @inheritDotParams diff_compare
#' @family Column match resolvers
#' @export
diff_cols_common <- function(x, ...) UseMethod("diff_cols_common")

diff_cols_common.data.frame <- function(x, .y, df_names = NULL, ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  diff_cols_common(diff_pair(x, .y, df_names, ...))
}

diff_cols_common.diff_pair <- function(z, ...) {
  purrr::reduce(metadata(z, "colnames"), intersect) %>%
    add_subclass("diff_cols_common")
}

diff_cols_common.diff_tbl <- function(z, ...) {
  purrr::reduce(metadata(z, "colnames"), intersect) %>%
    add_subclass("diff_cols_common")
}

print.diff_cols_common <- function(x) {
  print(remove_subclass(x, "diff_cols_common"))
}

# ---- diff_cols_unique ----

#' @title Unique columns
#' @inheritParams diff_cols_common
#' @inheritDotParams diff_compare
#' @family Column match resolvers
#' @export
diff_cols_unique <- function(x, ...) UseMethod("diff_cols_unique")

diff_cols_unique.data.frame <- function(x, .y, df_names = NULL, ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  diff_cols_unique(diff_pair(x, .y, df_names, ...))
}

diff_cols_unique.diff_pair <- function(z, ...) {
  uniq_cols <- list(x = c("x", "y"), y = c("y", "x")) %>%
    purrr::map(~ setdiff(metadata(z, "colnames")[[.[1]]], metadata(z, "colnames")[[.[2]]]))

  attr(uniq_cols, "df_names") <- metadata(z, "names")

  add_subclass(uniq_cols, "diff_cols_unique")
}

print.diff_cols_unique <- function(x) {
  cli::cat_line("Columns in ", attributes(x)$df_names[1], " and not in ", attributes(x)$df_names[2])
  print(x[[1]])
  cli::cat_line("Columns in ", attributes(x)$df_names[2], " and not in ", attributes(x)$df_names[1])
  print(x[[2]])
}


# ---- diff_cols_type -----

#' @title Column types
#' @inheritParams diff_cols_common
#' @inheritDotParams diff_compare
#' @family Column match resolvers
#' @export
diff_cols_type <- function(x, ...) UseMethod("diff_cols_type")

diff_cols_type.data.frame <- function(x, .y, df_names = NULL, ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  diff_cols_type(diff_pair(x, .y, df_names, ...))
}

diff_cols_type.diff_pair <- function(z, ...) {
  ct <- purrr::map(z[c("x", "y")], ~purrr::map(., ~class(.)[1])) %>%
    purrr::map_dfr(~., .id = "set") %>%
    tidyr::gather(variable, type, -set) %>%
    mutate(set = paste0("type.", set)) %>%
    tidyr::spread(set, type)

  attr(ct, "df_names") <- metadata(z, "names")

  add_subclass(ct, "diff_cols_type")
}

print.diff_cols_type <- function(x) {
  colnames(x)[2:3] <- attr(x, "df_names")
  print(remove_subclass(x, "diff_cols_type"))
}
