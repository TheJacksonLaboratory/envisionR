# Unit tests for the weighted_sem() function.
# Constructing an artificial dataset with a weighted SEM of 2
x <- c(0, 0, 12, -12, 0, 0, 0, -3, -3, 3, 3)
wts <- c(3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1)

test_that("weighted_sem() returns expected value", {
  # Getting required packages
  expect_equal(
    weighted_sem(x = x, w = wts, na.rm = TRUE),
    2
  )
})

test_that("weighted_sem() returns expected value with NAs and na.rm as TRUE", {
  # Getting required packages
  expect_equal(
    weighted_sem(x = c(x, NA), w = c(wts, NA), na.rm = TRUE),
    2
  )
})

test_that("weighted_sem() returns expected value with and na.rm as FALSE", {
  # Getting required packages
  expect_equal(
    weighted_sem(x = c(x, NA), w = c(wts, NA), na.rm = FALSE),
    as.numeric(NA)
  )
})
