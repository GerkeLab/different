#' Creates two nearly similar data sets with a varying number of data types.
#' In general, `x` is the reference and `y` is the corrupted data set.

make_char   <- function(iter, len) sapply(1:iter, function(x) paste0(sample(letters, len, replace = TRUE), collapse = ""))
make_factor <- function(iter, len) sapply(1:iter, function(x) factor(sample(letters[1:len], 1), levels = letters[1:len]))

make_core_fake <- function(n, rows = 100) {
  x <- list()
  for (i in 1:n) {
    type <- sample(c("int", "dbl", "char", "factor"), 1)
    x[[sprintf("colname_%02d", i)]] <- switch(
      type,
      int    = sample(-50:50, rows, replace = TRUE),
      dbl    = runif(rows, 0, 1) * 10^(sample(1:5, 1)),
      char   = make_char(rows, 6),
      factor = make_factor(rows, 10)
    )
  }
  tibble::as_tibble(x)
}

factorize <- function(x) {
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

add_ids <- function(df, n_ids = NULL) {
  x <- list()
  rows <- nrow(df)
  n <- if (is.null(n_ids)) factorize(rows) else n_ids
  types <- sample(c("int", "char"), length(n), replace = TRUE)
  for (i in 1:length(n)) {
    x[[sprintf("id_%02d", i)]] <- switch(
      types[i],
      int = sample(1:rows, n[i], replace = TRUE),
      char = make_char(n[i], 6)
    )
  }
  df_id <- if (is.null(n_ids)) {
    expand.grid(x)
  } else {
    x <- lapply(x, function(k) rep(k, ceiling(rows/length(k)))[1:rows])
  }
  x <- dplyr::bind_cols(df_id, df)
  tibble::as_tibble(x)
}

corrupt_values <- function(df, ..., n_rows = nrow(df)/5) {
  vars <- rlang::enexprs(...)
  vars <- tidyselect::vars_select(names(df), !!!vars)
  for (var in vars) {
    df[[var]][sample(1:nrow(df), n_rows)] <- sample(df[[var]], n_rows)
  }
  df
}

#' Make example data
example <- list()
example$x <- make_core_fake(10, 10^3)
example$x <- add_ids(example$x, n_ids = c(50^4, 600))
example$y <- corrupt_values(example$x, dplyr::contains("colname"), n_rows = 20)

example$x <- example$x[, sample(-3:-13, 1)]
example$y <- example$y[, sample(-3:-13, 2)]

#' Coerce some integer columns to character
y_ints <- sapply(example$y, is.integer)
y_ints[1:2] <- rep(FALSE, 2)
if (any(y_ints)) {
  example$y[[which(y_ints)[1]]] <- as.character(example$y[[which(y_ints)[1]]])
}

#' Create second example with unordered rows and varying numbers of rows
example2 <- purrr::map(
  example,
  ~ dplyr::group_by(., id_01, id_02)
)
example2 <- purrr::map(
  example2,
  ~ .[sample(1:nrow(.), floor(nrow(.) * runif(1, 0.992, 0.999))), ]
)
