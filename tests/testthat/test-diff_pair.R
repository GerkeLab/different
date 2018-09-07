context("test-diff_pair")

test_that("inputs are coerced to tibbles", {
  m <- list()
  m$df <- mtcars
  m$df_no_names <- m$df
  colnames(m$df_no_names) <- NULL
  m$matrix <- as.matrix(mtcars)
  m$matrix_no_names <- m$matrix
  colnames(m$matrix_no_names) <- NULL
  m$tbl <- tibble::as.tibble(mtcars)
  m$tlb_no_rownames <- m$tbl
  rownames(m$tbl_no_rownames) <- NULL

  n_row <- nrow(mtcars)
  n_col <- ncol(mtcars) + 1 # for row names

  expect_things <- function(x, n_row, n_col) {
    has_colnames <- !is.null(colnames(x))
    has_rownames <- !is.null(rownames(x))
    expected_cols <- if (has_colnames) colnames(x) else paste0("V", seq_len(n_col - 1))
    if (has_rownames) {
      expected_cols <- c(".row.names", expected_cols)
      expected_rows <- rownames(x)
    } else {
      n_col <- n_col - 1
    }

    x_class <- class(x)[1]
    x <- different:::coerce_tibble(x)

    expect_equal(nrow(x), n_row, info = x_class)
    expect_equal(ncol(x), n_col, info = x_class)
    expect_equal(colnames(x), expected_cols, info = x_class)
    if (has_rownames) expect_equal(x$`.row.names`, expected_rows, info = x_class)
  }

  # Something

  purrr::walk(m, expect_things, n_row, n_col)
})
