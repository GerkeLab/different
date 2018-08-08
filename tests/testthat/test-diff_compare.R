context("test-diff_compare")

test_that("diff_tbl consistent names metadata", {
  change_names <- function(...) {
    metadata(diff_compare(iris, iris, df_names = c(...)))$names
  }

  std_names <- c(x = "x", y = "y")

  expect_equal(change_names("x", "y"), std_names)
  expect_equal(names(change_names(y = "x", x = "y")), names(std_names))
  expect_equal(change_names("this" = 'x', "that" = 'y'), std_names)
  expect_warning(change_names("this" = 'x', "that" = 'y'))
  expect_equal(change_names("y" = "this", x = "that"),
               c(x = "that", y = "this"))
})
