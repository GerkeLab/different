context("test-dummy_data")

n_distinct_rows <- function(x, ...) {
  x %>%
    dplyr::select(!!!rlang::enexprs(...)) %>%
    dplyr::distinct() %>%
    nrow()
}

test_that("dummy helpers give correct dimensions and class", {
  x_chr <- dummy_chr(10, chr_len = 10)
  x_fct <- dummy_fct(10, fct_n_levels = 6)
  x_int <- dummy_int(10, int_min = 0, int_max = 10)
  x_dbl <- dummy_dbl(10)
  expect_is(x_chr, "character")
  expect_is(x_fct, "factor")
  expect_is(x_int, "integer")
  expect_is(x_dbl, "numeric")

  expect_equal(nchar(x_chr), rep(10, 10))
  expect_equal(length(levels(x_fct)), 6)
  expect_true(min(x_int) >= 0)
  expect_true(max(x_int) <= 10)

  x_dbl <- dummy_dbl(10, runif, min = 1, max = 2)
  expect_true(min(x_dbl) >= 1)
  expect_true(max(x_dbl) <= 2)
})

test_that("dummy_col dispatches correctly", {
  x_chr <- dummy_col(10, type = "chr", chr_len = 10, int_min = 0)
  x_fct <- dummy_col(10, type = "fct", fct_n_levels = 6, chr_len = 10)
  x_int <- dummy_col(10, type = "int", int_min = 0, int_max = 10, fct_n_levels = 6)
  x_dbl <- dummy_col(10, type = "dbl", rbeta, shape1 = 1, shape2 = 5)
  expect_is(x_chr, "character")
  expect_is(x_fct, "factor")
  expect_is(x_int, "integer")
  expect_is(x_dbl, "numeric")

  expect_equal(nchar(x_chr), rep(10, 10))
  expect_equal(length(levels(x_fct)), 6)
  expect_true(min(x_int) >= 0)
  expect_true(max(x_int) <= 10)

  x_dbl <- dummy_col(10, type = "dbl", runif, min = 1, max = 2, int_min = 0)
  expect_true(min(x_dbl) >= 1)
  expect_true(max(x_dbl) <= 2)
})

test_that("factorize returns prime factors", {
  x <- c(2, 2, 3, 5, 7, 13)
  fx <- factorize(purrr::reduce(x, `*`))
  expect_equal(fx, x)

  x <- c(2, 3, 7, 17, 29)
  fx <- factorize(purrr::reduce(x, `*`))
  expect_equal(fx, x)
})

test_that("enough factors does its thing", {
  x <- c(2, 2, 3, 5, 7, 13)
  x <- purrr::reduce(x, `*`)

  expect_length_and_value <- function(x, nf) {
    fx <- enough_factors(x, nf)
    expect_equal(length(fx), nf)
    expect_equal(purrr::reduce(fx[!is.na(fx)], `*`), x)
  }

  purrr::walk(1:8, expect_length_and_value, x = x)
})


test_that("dummy_data has the right shape", {
  x <- dummy_data(n_col = 8, n_row = 100, types = c("int", "dbl", "chr", "fct"),  n_id_col = 1)
  expect_equal(ncol(x), 9)
  expect_equal(nrow(x), 100)
  y <- x %>%
    dplyr::select(dplyr::starts_with("col")) %>%
    dplyr::summarize_all(class) %>%
    tidyr::gather(col, type) %>%
    dplyr::count(type)
  expect_equal(y$n, rep(2, 4))

  expect_equal(n_distinct_rows(x, id_01), 100)

  x <- dummy_data(n_col = 10, n_row = 100, n_id_col = 2)
  expect_equal(n_distinct_rows(x, dplyr::starts_with("id")), 100)
  x <- dummy_data(n_col = 10, n_row = 100, n_id_col = 3) # Overspecifying IDs
  expect_equal(n_distinct_rows(x, dplyr::starts_with("id")), 100)

  expect_error(dummy_data(n_id_col = 0))
  expect_error(dummy_data(n_col = 0))
})

test_that("shuffle-corrupted dummy_data still has all keys", {
  n_row <- 100
  x <- dummy_data(n_row = n_row) %>% dummy_corrupt(shuffle = TRUE, dropout = 0, paired = FALSE)
  x_n_row <- n_distinct_rows(x, dplyr::starts_with("id"))
  expect_equal(x_n_row, n_row)

  x <- dummy_data(n_row = n_row) %>% dummy_corrupt(shuffle = TRUE, dropout = 0.1, paired = FALSE)
  x_n_row <- n_distinct_rows(x, dplyr::starts_with("id"))
  expect_true(dplyr::between(x_n_row, n_row * 0.9, n_row))
})

test_that("corrupt_type rate works", {
  x <- purrr::map(1:5000, ~ corrupt_type(dummy_dbl(10), 0)) %>% purrr::map_chr(class) %>% table()
  expect_equivalent(x[1], 5000)

  x <- purrr::map(1:10000, ~ corrupt_type(dummy_fct(10), 0.25)) %>% purrr::map_chr(class) %>% table()
  expect_true(dplyr::between(x["character"]/sum(x), 0.24, 0.26))
})
