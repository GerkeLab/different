context("test-not_equal")

test_that("not_equal() generally works", {
  left <- 1:10
  right <- left
  right[c(1, 5, 7)] <- 13:15
  expect_equal(not_equal(left, right), c(1, 5, 7))
})

test_that("not_equal() considers NA as different", {
  left <- 1:10
  right <- left
  right[c(1, 5, 7)] <- NA
  expect_equal(not_equal(left, right), c(1, 5, 7))
})

test_that("not_equal() considers NAs as equal", {
  left <- 1:10
  right <- left
  right[c(1, 5, 7)] <- NA
  left[c(1,5,7)] <- NA
  expect_equal(not_equal(left, right), integer(0))
})

test_that("not_equal() handles doubles/numeric", {
  left <- runif(10)
  right <- left + sample(-1:1, 10, replace = TRUE) * .Machine$double.eps * 0.5
  right[c(1, 5, 7)] <- right[c(1, 5, 7)] + .Machine$double.eps * 2
  expect_equal(not_equal(left, right), c(1, 5, 7))
})
