# unit test for the moving_average() function

ma_center <- c(rep(NA, times = 7), 8:53, rep(NA, times = 7))
ma_right <- c(rep(NA, times = 14), 8:53)
ma_left <- c(8:53, rep(NA, times = 14))

test_that("moving_average() works with center align", {
  # Getting required packages
  expect_equal(
    moving_average(x = 1:60, n = 15, align = "center"),
    ma_center
  )
})

test_that("moving_average() works with left align", {
  # Getting required packages
  expect_equal(
    moving_average(x = 1:60, n = 15, align = "left"),
    ma_left
  )
})

test_that("moving_average() works with right align", {
  # Getting required packages
  expect_equal(
    moving_average(x = 1:60, n = 15, align = "right"),
    ma_right
  )
})
