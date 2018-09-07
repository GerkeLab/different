context("test-diff_pair")

m <- list()
m$df <- mtcars
m$df_no_names <- m$df
colnames(m$df_no_names) <- NULL
m$matrix <- as.matrix(mtcars)
m$matrix_no_names <- m$matrix
colnames(m$matrix_no_names) <- NULL
m$tbl <- tibble::as.tibble(mtcars)
m$tbl_no_rownames <- m$tbl
rownames(m$tbl_no_rownames) <- NULL

test_that("inputs are coerced to tibbles", {
  n_row <- nrow(mtcars)
  n_col <- ncol(mtcars) + 1 # for row names

  expect_things <- function(x, n_row, n_col) {
    has_colnames <- !is.null(colnames(x))
    has_rownames <- if (tibble::is.tibble(x)) tibble::has_rownames(x) else !is.null(rownames(x))
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

test_that("diff_pair gives correct things", {
  expect_equal(diff_pair(m$matrix, m$df)$diff_meta$class, list(x = "matrix", y = "data.frame"))
  expect_equal(diff_pair(m$tbl, m$matrix)$diff_meta$class, list(x = c("tbl_df", "tbl", "data.frame"), y = "matrix"))
  # List input
  expect_equivalent(diff_pair(m[c("matrix", "df")]), diff_pair(m$matrix, m$df, df_names = c("matrix", "df")))
  expect_warning(diff_pair(m))     # warn for lists length > 2
  expect_error(diff_pair(m$df))    # error when no y
  expect_error(diff_pair(m["df"])) # error when lists length < 2
  expect_error(diff_pair(m$df, m$df$mpg), regexp = "compare data\\.frame \\(x\\) to numeric")
  expect_error(diff_pair(m$df$mpg),       regexp = "work with numeric")
  expect_error(diff_pair(m$df$mpg, m$df), regexp = "work with numeric")
  expect_error(diff_pair(m$matrix), regexp = "requires two")

  expect_equal(metadata(diff_pair(m$matrix, m$df), "names"), c("x" = "m$matrix", "y" = "m$df"))
  expect_equal(metadata(diff_pair(m[c("matrix", "df")]), "names"), c("x" = "matrix", "y" = "df"))
  expect_equal(metadata(diff_pair(list(m$matrix, m$df)), "names"), c("x" = "list(m$matrix, m$df)[[1]]", "y" = "list(m$matrix, m$df)[[2]]"))

  expect_equal(capture.output(print(diff_pair(m$df, m$df_no_names)))[1],
               "<diff_pair: m$df vs m$df_no_names>")
})
