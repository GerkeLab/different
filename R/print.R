cat_glue <- function(..., .envir = parent.frame()) cli::cat_line(glue(..., .envir = .envir))

cat_bullet <- function(..., .envir = parent.frame(), bullet = "bullet") {
  cli::cat_bullet(glue(..., .envir = .envir), bullet = bullet)
}

cat_header <- function(x) cli::cat_line(crayon::bold(crayon::cyan(x)))

subtle <- function(x) crayon::style(x, "#808080")


wrap_lines <- function(x, indent = 4) {
  # browser()
  goal_width <- cli::console_width() - indent
  if (nchar(x) <= goal_width) return(paste0(strrep(" ", indent), x))

  breaks <- dplyr::data_frame(
    idx = as.numeric(gregexpr("[ .,;-]", x)[[1]])
  ) %>%
    mutate(
      grp = cut(idx, breaks = c(0, seq(goal_width, goal_width * (nchar(x) %/% goal_width), by = goal_width), Inf))
    ) %>%
    group_by(grp) %>%
    summarize(split = max(idx)) %>%
    pull(split)
  breaks <- c(0, breaks, if (max(breaks) != nchar(x)) nchar(x))
  out <- c()
  for (i in seq_along(breaks)[-1]) {
    out <- c(out, substr(x, breaks[i-1]+1, breaks[i]))
  }
  out <- gsub("^\\s*", "", out)
  out <- paste0(strrep(" ", indent), out, sep = "\n")
}

cat_differences <- function(z) {
  n_diffs <- sum(z$n_diff, na.rm = TRUE)
  n_unique_columns_x <- sum(z$state == "unique_x")
  n_unique_columns_y <- sum(z$state == "unique_y")
  if (!n_diffs) {
    if (n_unique_columns_x + n_unique_columns_y) {
      cat_bullet("There were no differences in overlapping columns between {paste0('`', metadata(z)$names, '`', collapse = ' and ')}", bullet = "tick")
    } else {
      cat_bullet("There were no differences found between {paste0('`', metadata(z)$names, '`', collapse = ' and ')}", bullet = "tick")
    }
  } else {
    n_diff_rows <- purrr::map(z$diff, "miss_index") %>%
      purrr::reduce(union) %>% length()
    n_diffs <- glue("{crayon::bold(n_diffs)} differences")
    cat_bullet("There were {crayon::red(n_diffs)} across {sum(z$state == 'diff')} cols and {n_diff_rows} rows", bullet = "cross")
  }
}

cat_overlapping_columns <- function(z) {
  stopifnot(is_diff_tbl(z))
  overlaps <-
    z %>%
    filter(
      !grepl("unique", state),
      !grepl("^_row", variable)
    ) %>%
    group_by(state) %>%
    summarize(
      vars = paste0("`", crayon::bold(variable), "`", collapse = ", "),
      n = n()
    )

  noun <- pluralize(sum(overlaps$n), "column")
  cat_bullet("{crayon::bold(sum(overlaps$n))} {noun} appear in both {df_name(z, 'x', 0.3)} and {df_name(z, 'y', 0.3)}")

  if ("same" %in% overlaps$state) {
    same_cols <- overlaps %>% filter(state == "same")
    noun <- pluralize(same_cols$n, "column")
    cli::cat_line(crayon::green(glue("  \U2714 {crayon::bold(same_cols$n)} {noun} have identical entries: ")))
    cat_variable_names(z, "same")
  }
  if ("diff" %in% overlaps$state) {
    diff_cols <- overlaps %>% filter(state == "diff")
    noun <- pluralize(diff_cols$n, "column")
    cli::cat_line(crayon::red(glue("  \u2716 {crayon::bold(diff_cols$n)} {noun} have differences: ")))
    cat_variable_names(z, "diff", 4)
  }
}

cat_variable_names <- function(z, state = "diff", indent = 4) {
  filter(z, state == !!state) %>%
    pull(variable) %>%
    {glue("`{crayon::bold(.)}`")} %>%
    glue_collapse(sep = ", ") %>%
    subtle() %>%
    wrap_lines(indent = indent) %>%
    cat(sep = "")
  cat("\n")
}

cat_unique_columns <- function(z) {
  n_unique_columns_x <- sum(z$state == "unique_x")
  n_unique_columns_y <- sum(z$state == "unique_y")

  if (n_unique_columns_x) {
    noun <- pluralize(n_unique_columns_x, "column")
    verb <- pluralize(n_unique_columns_x, "is", "are")
    cat_bullet("{crayon::bold(n_unique_columns_x)} {noun} in {crayon::bold(df_name(z, 'x'))} {verb} not in {df_name(z, 'y')}:")
    cat_variable_names(z, "unique_x")
  }
  if (n_unique_columns_y) {
    noun <- pluralize(n_unique_columns_y, "column")
    verb <- pluralize(n_unique_columns_y, "is", "are")
    cat_bullet("{crayon::bold(n_unique_columns_y)} {noun} in {crayon::bold(df_name(z, 'y'))} {verb} not in {df_name(z, 'x')}:")
    cat_variable_names(z, "unique_y")
  }
}

cat_dimensions <- function(z) {
  dims <- purrr::map_dfr(setNames(metadata(z, "dims"), metadata(z, "names")),
    ~ data_frame(rows = .[1], cols = .[2]), .id = "set")

  dims <- capture_tibble_print(dims, underline = TRUE)
  cli::cat_line(dims)
  cli::cat_line()
}

df_name <- function(z, name_of = "x", truncate_width = 0.45) {
  nm <- metadata(z, "names")[[name_of]]
  if (nchar(nm) > (cli::console_width() * truncate_width)) name_of else nm
}

pluralize <- function(n, base, pluralized = paste0(base, "s")) {
  if (n < 2) base else pluralized
}
