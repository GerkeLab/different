#' Create a new diff_tbl
#'
#' @keywords internal
new_diff_tbl <- function(x, metadata = NULL, ..., subclass = NULL) {
  tibble::new_tibble(
    x,
    diff_meta = metadata,
    ...,
    nrow = length(x$variable),
    subclass = c(subclass, "diff_tbl")
  )
}

is_diff_tbl <- function(x) {
  inherits(x, "diff_tbl")
}

un_diff_tbl <- function(x) {
  structure(x, class = setdiff(class(x), "diff_tbl"))
}

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' @method as_tibble diff_tbl
#' @export
as_tibble.diff_tbl <- function(x, ...) {
  tibble::as_tibble(un_diff_tbl(x), ...)
}

#' @export
metadata <- function(x, ...) UseMethod("metadata", x)

as.data.frame.diff_tbl <- function(x, ...) {
  as.data.frame(un_diff_tbl(x), ...)
}

#' @method print diff_tbl
#' @export
print.diff_tbl <- function(z) {
  cat_glue(subtle("<diff_tbl: {paste(metadata(z, 'names'), collapse = ' vs ')}>"))
  cat_differences(z)
}

# ---- metadata ----

#' Get metadata from different object
#'
#' Helper to pull out metadata from the [diff_tbl] objects output by [diff_compare()].
#' @export
metadata <- function(x, ...) UseMethod("metadata", x)

#' @method metadata diff_tbl
#' @export
metadata <- function(x, ...) UseMethod("metadata", x)
metadata.diff_tbl <- function(z, prop = NULL) {
  meta <- attributes(z)$diff_meta
  if (!is.null(prop)) {
    prop <- match.arg(prop, names(meta))
    meta[[prop]]
  } else meta
}

new_metadata <- function(x, y, df_names, df_class = list(x = class(x), y = class(y)), ...) {
  stopifnot(inherits(x, "data.frame"), inherits(y, "data.frame"))
  df_names <- xy_named_ordered(df_names)
  df_class <- xy_named_ordered(df_class)

  list(
    names = df_names[c("x", "y")],
    class = df_class,
    dims = purrr::map(list(x = x, y = y), ~ dim(.)),
    colnames = purrr::map(list(x = x, y = y), ~ colnames(.)),
    ...
  )
}

xy_named_ordered <- function(x, var_name) {
  if (is.null(names(x)) && length(x) == 2) {
    names(x) <- c("x", "y")
  }
  if (!"x" %in% names(x) || !"y" %in% names(x)) {
    if (length(x) != 2) abort(glue("`{var_name}` must be length 2 or named with 'x' and 'y'"))
    names(x)[which(!names(x) %in% c("x", "y"))] <- setdiff(c("x", "y"), names(x))
  }
  x
}

# ---- summary ----

#' @method summary diff_tbl
#' @export
summary.diff_tbl <- function(z) {
  cli::cat_rule("different: Comparison Summary")
  cat_header("# Dimensions")
  cat_dimensions(z)
  cat_header("# Columns")
  cat_unique_columns(z)
  cat_overlapping_columns(z)
  cat_header("# Differences")
  cat_differences(z)
  cat(capture_tibble_print(z, underline = TRUE))
}

# ---- Plot ----

#' @export
vis_changed <- function(x, y, ...) {
  z <- tidy_diff(x, y, ...)
  plot(z)
}

#' @method plot diff_tbl
#' @export
plot.diff_tbl <- function(z) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  x_row_ids <- 1:metadata(z, "dims")$x[1]

  missingness_names <- c(
    paste("Missing in", metadata(z)$names["x"]),
    paste("Missing in", metadata(z)$names["y"]),
    "Different"
  )

  # Create labels for plotly
  z_labels <- z$diff %>%
    purrr::set_names(nm = z$variable) %>%
    purrr::compact() %>%
    purrr::map_dfr(
      .id = "variable",
      ~ mutate(
        ., label = glue::glue_data(., "{metadata(z)$names[1]}: {value.x}\n{metadata(z)$names[2]}: {value.y}\n"),
        label = paste(label),
        missingness = case_when(
          is.na(value.x) ~ missingness_names[1],
          is.na(value.y) ~ missingness_names[2],
          TRUE ~ missingness_names[3]
        )
      ) %>%
        select(-value.x:-value.y)
    )
  # Hacky but I don't know the ID var names ahead of time
  # So I rely on these columns being appended to the z$tidy dataframes because
  # they're the same across all columns. The above chunk adds `label` column to
  # the end so I use it and `miss_index` as anchor points to grab a section that
  # will certainly include the id columns, then strip the things that are
  # guaranteed to be there, even if the id columns aren't.
  z_id_vars <- tidyselect::vars_select(names(z_labels), miss_index:label)
  z_id_vars <- z_id_vars[c(-length(z_id_vars), -1)] # remove miss_index and label
  if (length(z_id_vars)) {
    z_labels$id_labels <- apply(z_labels, 1, function(x) {
      label <- ""
      for (id in z_id_vars) {
        label <- paste0(label, "\n", id, ": ", x[id])
      }
      label
    })
  } else z_labels$id_labels <- ""
  z_labels <- z_labels %>%
    mutate(label = paste0(label, id_labels)) %>%
    select(variable, miss_index, label, missingness)

  missingness_colors <- setNames(c("#1F78B4", "#33A02C", "#E31A1C"), missingness_names)

  z %>%
    filter(state == "diff") %>%
    mutate(misses = purrr::map(diff, 'miss_index')) %>%
    select(variable, state, id = misses) %>%
    tidyr::unnest() %>%
    left_join(z_labels, by = c("variable", id = "miss_index")) %>%
    {
      ggplot2::ggplot(.) +
        ggplot2::aes(x = variable, y = -id, color = missingness, text = label) +
        ggplot2::geom_point(shape = 15) +
        ggplot2::scale_color_manual(values = missingness_colors) +
        ggplot2::scale_y_continuous(labels = function(x) abs(x), expand = c(0.04,0)) +
        ggplot2::scale_x_discrete(position = "top", expand = c(0.04,0)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x.top = ggplot2::element_text(angle = 45, vjust = 0, hjust = 0),
          legend.position = "bottom"
        ) +
        ggplot2::labs(x = "Column", y = "Row", color = NULL)
    }
}
