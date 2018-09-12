#' Create Dummy Data
#'
#' @inheritParams dummy_data_core
#' @inheritParams dummy_add_ids
#' @export
dummy_data <- function(
  n_row = 100,
  n_col = 10,
  types = sample(c("int", "dbl", "fct", "chr"), n_col, replace = TRUE),
  n_id_col = 2,
  id_names = NULL,
  ...
) {
  x <- dummy_data_core(n_row = n_row, n_col = n_col, types = types, ...)
  if (n_id_col < 1) return(x)
  dummy_add_ids(x, n_id_col = n_id_col, id_names = id_names)
}

#' @title Generate Core Dummy Data
#' @param n_col `<int>` Number of columns
#' @param n_row `<int>` Number of rows
#' @param types `<chr>` Data types of the columns, recycled or truncated to
#'   match `n_col`, or by default chosen at random. If named, column names will
#'   match the names of this vector.
#' @inheritDotParams dummy_col
dummy_data_core <- function(
  n_row = 100,
  n_col = 10,
  types = sample(c("int", "dbl", "fct", "chr"), n_col, replace = TRUE),
  ...
) {
  stopifnot(n_col > 0L, n_row> 0L)
  if (length(types) < n_col) {
    types <- rep(types, ceiling(n_col / length(types)))
  }
  if (length(types) > n_col) {
    types <- types[1:n_col]
  }
  if (is.null(names(types))) {
    names(types) <- sprintf("col_%02d", seq_len(n_col))
  }
  purrr::map_dfc(types, dummy_col, n = n_row, ...) %>%
    tibble::as_tibble()
}

#' @title Add IDs to Dummy Data Core
#' @param df `<tbl>` Input data frame to which ID collumns shold be addeed
#' @param n_id_col `<int>` Number of ID columns to add
#' @param id_names `<chr>` Vector of names for ID columns
dummy_add_ids <- function(df = dummy_data_core(), n_id_col = 2, id_names = NULL) {
  stopifnot(n_id_col > 0)
  n_lvls <- enough_factors(nrow(df), n_id_col)
  n_lvls[is.na(n_lvls)] <- rbinom(length(n_lvls[is.na(n_lvls)]), nrow(df), runif(1, 0.25, 0.75))
  n_lvls <- n_lvls[n_lvls > 0]
  ids <- if (length(n_lvls)> 1) {
    purrr::map(n_lvls, dummy_lvls) %>%
      expand.grid()
  } else {
    dplyr::data_frame(id = dummy_lvls(n_lvls))
  }

  ids <- ids[sample(1:nrow(df), nrow(df)), ]
  names(ids) <- if (!is.null(id_names)) id_names else sprintf("id_%02d", seq_len(n_id_col))
  dplyr::bind_cols(ids, df) %>%
    dplyr::as_tibble()
}

#' @title Corrupt Dummy Data
#' @param df `<tbl>` Data frame to be corrupted
#' @param ... Columns to be included for corruption, unquoted following
#'   [dplyr::select()] semantics.
#' @param n_corrupted `<int>` Number of rows to be corrupted within each
#'   selected column.
#' @param rx_exclude `<regexp>` Regular expression indicating which columns
#'   should be protected from corruption. Set to `NULL` to disable.
#' @param shuffle `<lgl>` If `TRUE` (default), rows are suffled
#' @param corrupt_type_rate `<dbl>` Rate between 0 and 1 at which column type
#'   should be corrupted. A rate of 0.25 means that each column will have a 25%
#'   chance of being corrupted.
#' @param dropout `<dbl>` Percentage of rows/columns to drop (default value of
#'   `dropout_col` or `dropout_row` unless those values are otherwise
#'   specified.)
#' @param dropout_col `<dbl>` Percentage of columns to drop
#' @param dropout_row `<dbl>` Percentage of rows to drop
#' @param paired `<lgl>` If `TRUE` (default), returns a [diff_pair()] with the
#'   original and corrupted data sets.
#' @export
dummy_corrupt <- function(
  df = dummy_data(), ...,
  n_corrupted = nrow(df)/5,
  rx_exclude = "id|ID",
  shuffle = TRUE,
  corrupt_type_rate = 0.25,
  dropout = 0.1,
  dropout_col = dropout,
  dropout_row = dropout,
  paired = TRUE
) {
  vars <- enexprs(...)
  vars <- tidyselect::vars_select(names(df), !!!vars)
  if (length(vars) == 0) vars <- names(df)
  if (!is.null(rx_exclude)) vars <- vars[!grepl(rx_exclude, vars)]
  df2 <- df
  for (var in vars) {
    df2[[var]][sample(1:nrow(df2), n_corrupted)] <- sample(df2[[var]], n_corrupted)
    df2[[var]] <- corrupt_type(df[[var]], corrupt_type_rate)
  }
  keep_rows <- sample(1:nrow(df2), nrow(df2) * (1 - dropout_row))
  keep_vars <- setdiff(names(df), vars)
  keep_vars <- c(keep_vars, sample(vars, length(vars) * (1 - dropout_col)))
  df2 <- if (shuffle) {
    df2[keep_rows, keep_vars]
  } else {
    keep_vars <- intersect(names(df), keep_vars)
    df2[sort(keep_rows), keep_vars]
  }
  if (paired) diff_pair(df, df2) else df2
}

corrupt_type <- function(x, rate) {
  if (runif(1) < (1 - rate)) return(x)
  switch(
    class(x)[1],
    "integer" = as.character(x),
    "factor" = as.character(x),
    "numeric" = as.character(x),
    "character" = factor(x, unique(x)),
    x
  )
}

#' Dummy Column
#'
#' @param n Number of elements
#' @param type One of `"int"`, `"dbl"`, `"chr"` or `"fct"`
#' @param ... Arguments passed to associated helper functions. All helper
#'   `dummy_col()` functions ignore extra parameters, except for `dummy_dbl()`
#'   where additional parameters are passed to `dbl_rfun`.
#' @export
dummy_col <- function(n, type = c("int", "dbl", "chr", "fct"), ...) {
  dummy_fun <- switch(
    match.arg(type),
    "int" = dummy_int,
    "dbl" = dummy_dbl,
    "chr" = dummy_chr,
    "fct" = dummy_fct
  )
  # Clean up dots for the helper functions
  dots <- list(...)
  if (type == "dbl") {
    # dummy_dbl takes extra arguments
    other_types <- setdiff(eval(formals(dummy_col)$type), type)
    rx_others <- paste0("^(", paste0(other_types, collapse = "|"), ")")
    dots <- dots[!grepl(rx_others, names(dots))]
  } else {
    # the rest do not
    dots <- dots[grepl(type, names(dots))]
  }

  dots$n <- n
  do.call(dummy_fun, dots)
}

#' @rdname dummy_col
#' @param chr_len `<int>` Length of strings
#' @examples
#' dummy_chr(10, chr_len = 4)
#' @export
dummy_chr <- function(n, chr_len = 6) {
  replicate(n, dummy_str(chr_len))
}

#' @rdname dummy_col
#' @param fct_n_levels `<int>` Number of factor levels
#' @examples
#' dummy_fct(10, fct_n_levels = 5)
#' @export
dummy_fct <- function(n, fct_n_levels = round(n * 0.333, 0)) {
  lvls <- dummy_lvls(fct_n_levels)
  factor(sample(lvls, n, replace = TRUE), levels = lvls)
}

#' @rdname dummy_col
#' @param int_min `<int>` Minimum integer value
#' @param int_max `<int>` Maximum integer value
#' @examples
#' dummy_int(10)
#' dummy_int(10, int_min = 1, int_max = 3)
#' @export
dummy_int <- function(n, int_min = -50L, int_max = 50L) {
  sample(seq(int_min, int_max), n, replace = TRUE)
}

#' @rdname dummy_col
#' @param dbl_rfun `<fun>` A function that takes an argument `n` and generates
#'   a vector of `n` random doubles.
#' @examples
#' dummy_dbl(10)
#' dummy_dbl(10, runif, min = 1, max = 10)
#' @export
dummy_dbl <- function(n, dbl_rfun = function(n, ...) {runif(n, 0, 1) * 10^sample(1:5, 1)}, ...) {
  dbl_rfun(n, ...)
}

dummy_str <- function(str_len) {
  paste0(sample(letters, str_len, replace = TRUE), collapse = "")
}

dummy_lvls <- function(n) {
  x <- letters
  if (n <= 26) return(x[seq_len(n)])
  for (i in seq(1, floor(log(n, 26)))) {
    x <- purrr::map(x, ~ paste0(., letters)) %>%
      purrr::simplify()
  }
  sort(x)[1:n]
}

factorize <- function(x) {
  stopifnot(length(x) == 1)
  x <- as.integer(x)
  div <- seq_len(abs(x))[-1]
  all_fct <- div[x %% div == 0L]
  factors <- c()
  flag <- TRUE
  while(flag) {
    this <- all_fct[x %% all_fct == 0L]
    if (!length(this)) {
      flag <- FALSE
    } else {
      factors <- c(factors, this[1])
      x <- x / this[1]
    }
  }
  return(factors)
}

enough_factors <- function(x, n_factors = 2L) {
  fx <- factorize(x)
  if (length(fx) < n_factors) return(c(fx, rep(NA_integer_, n_factors - length(fx))))
  group <- seq_along(fx) %% n_factors + 1
  purrr::map(1:n_factors, ~ fx[which(group == .)]) %>%
    purrr::map_dbl(~ purrr::reduce(., `*`))
}
