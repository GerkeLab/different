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

#' @export
diff_cols_common.data.frame <- function(x, .y, df_names = NULL, ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  diff_cols_common(diff_pair(x, .y, df_names, ...))
}

#' @export
diff_cols_common.diff_pair <- function(z, ...) {
  purrr::reduce(metadata(z, "colnames"), intersect) %>%
    add_subclass("diff_cols_common")
}

#' @export
diff_cols_common.diff_tbl <- function(z, ...) {
  purrr::reduce(metadata(z, "colnames"), intersect) %>%
    add_subclass("diff_cols_common")
}

#' @export
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

#' @export
diff_cols_unique.data.frame <- function(x, .y, df_names = NULL, ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  diff_cols_unique(diff_pair(x, .y, df_names, ...))
}

#' @export
diff_cols_unique.diff_pair <- function(z, ...) {
  uniq_cols <- list(x = c("x", "y"), y = c("y", "x")) %>%
    purrr::map(~ setdiff(metadata(z, "colnames")[[.[1]]], metadata(z, "colnames")[[.[2]]]))

  attr(uniq_cols, "df_names") <- metadata(z, "names")

  add_subclass(uniq_cols, "diff_cols_unique")
}

#' @export
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

#' @export
diff_cols_type.data.frame <- function(x, .y, df_names = NULL, ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  diff_cols_type(diff_pair(x, .y, df_names, ...))
}

#' @export
diff_cols_type.diff_pair <- function(z, ...) {
  ct <- purrr::map(z[c("x", "y")], ~purrr::map(., ~class(.)[1])) %>%
    purrr::map_dfr(~., .id = "set") %>%
    tidyr::gather(variable, type, -set) %>%
    mutate(set = paste0("type.", set)) %>%
    tidyr::spread(set, type)

  attr(ct, "df_names") <- metadata(z, "names")

  add_subclass(ct, "diff_cols_type")
}

#' @export
print.diff_cols_type <- function(x) {
  colnames(x)[2:3] <- attr(x, "df_names")
  print(remove_subclass(x, "diff_cols_type"))
}

# ---- diff_cols_pmatch ----

#' @title Use Partial Matching to Find Common Column Names
#' @inheritParams diff_cols_common
#' @inheritDotParams diff_pair
#' @family Column match resolvers
#' @export
diff_cols_pmatch <- function(x, ...) UseMethod("diff_cols_pmatch")

#' @export
diff_cols_pmatch.data.frame <- function(x, .y, df_names = NULL, ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  diff_cols_pmatch(diff_pair(x, .y, df_names, ...))
}

#' @export
diff_cols_pmatch.diff_pair <- function(z, ...) {
  x_names <- names(z$x)
  y_names <- names(z$y)
  idx_x2y_match <- pmatch(x_names, y_names, duplicates.ok = TRUE)
  idx_y2x_match <- pmatch(y_names, x_names, duplicates.ok = TRUE)

  xy_matches <- tibble(
    column_x = x_names,
    column_y = y_names[idx_x2y_match]
  ) %>%
    filter(!is.na(column_y))

  yx_matches <- tibble(
    column_y = y_names,
    column_x = x_names[idx_y2x_match]
  ) %>%
    filter(!is.na(column_x)) %>%
    # prefer matches from `x` columns
    anti_join(xy_matches, by = "column_x")

  bind_rows(xy_matches, yx_matches) %>%
    full_join(tibble(column_x = x_names), by = "column_x") %>%
    full_join(tibble(column_y = y_names), by = "column_y")
}

# ---- diff_cols_match ----

#' @title Match Column Names
#' @inheritParams diff_cols_common
#' @inheritDotParams diff_pair
#' @family Column match resolvers
#' @export
diff_cols_match <- function(x, .rename = NULL, ...) UseMethod("diff_cols_match")

#' @export
diff_cols_match.data.frame <- function(x, .y, df_names = NULL, ...) {
  df_names <- df_names[1:2] %||% paste(sys.call())[2:3]
  diff_cols_match(diff_pair(x, .y, df_names, ...))
}

#' @export
diff_cols_match.diff_pair <- function(
  z,
  .rename = NULL,
  ...,
  .method = c("pmatch")
) {
  # rename can be...
  # * list of renaming list(col_in_x = col_in_y)
  # * data frame with columns "column_x", "column_y"
  #
  # dots are col_in_x = col_in_y
  if (is.null(.rename)) {
    .rename <- switch(
      match.arg(.method),
      "pmatch" = diff_cols_pmatch(z)
    )
  }

  if (is_strictly_list(.rename)) {
    list_only_length_one(.rename, "diff_cols_match_bad_rename_format")
    .rename <- purrr::imap_dfr(.rename, ~ tibble(column_x = .y, column_y = .x))
  }

  if (!inherits(.rename, "data.frame")) {
    abort(
      ".rename must be a list or data.frame. ",
      "If `.rename` is a list, elements are formatted: ",
      "`list(col_name_in_x = 'col_name_in_y')`. ",
      "If `.rename` is a data.frame, it must have two columns 'column_x' and ",
      "corresponding name 'column_y'.",
      .subclass = "diff_cols_match_bad_rename_format"
    )
  } else {
    if (length(setdiff(c("column_x", "column_y"), names(.rename))) != 0) {
      abort(
        "`.rename` must have columns 'column_x' and 'column_y' containing ",
        "column names in `x` ({metadata(z, 'names')['x']}) and ",
        "respective column names in `y` ({metadata(z, 'names')['y']})",
        .subclass = "diff_cols_match_bad_rename_format"
      )
    }
  }

  # dots overwrite .rename. warn on clash?
  dots <- list(...)
  if (length(dots)) {
    dots <- purrr::imap_dfr(dots, ~ tibble(column_x = .y, column_y = .x))

    .rename <- bind_rows(
      dots,
      anti_join(.rename, dots, by = "column_x")
    )
  }

  # can't rename missing values
  .rename <- filter_at(.rename, quos(column_x, column_y), ~ !is.na(.))

  # TODO: store transformation instead of modifying
  new_y_names <- purrr::set_names(.rename$column_y, .rename$column_x)
  z$y <- rename(z$y, !!!new_y_names)
  z$diff_meta$colnames$y <- colnames(z$y)
  z
}

list_only_length_one <- function(x, .subclass = "diff_list_length_must_be_one") {
  if (!is_strictly_list(x)) return(invisible())

  len_just_one <- purrr::map_lgl(x, ~ length(.) == 1)
  if (all(len_just_one)) return(invisible(TRUE))

  bad_element <- names(x)[!len_just_one][1]
  abort(
    "Rename elements must be length 1. '{bad_element}' had ",
    "{ifelse(length(x[[bad_element]]) > 1, 'more', 'less')} than ",
    "one element.",
    .subclass = .subclass,
    bad_elements = names(x)[!len_just_one]
  )
}
