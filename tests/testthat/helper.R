expect_between <- function(result, lower_bound, upper_bound) {
  result      <- rlang::enquo(result)
  lower_bound <- rlang::enquo(lower_bound)
  upper_bound <- rlang::enquo(upper_bound)
  expect_lte(!!result, !!upper_bound)
  expect_gte(!!result, !!lower_bound)
}
