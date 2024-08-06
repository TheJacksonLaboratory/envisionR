# unit tests for velocity_unit_convert()

# Testing all distances as both numerator and denominator
test_that("velocity_unit_convert() converts from cm to mm", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/s",
      units_out = "mm/s"
    ),
    c(10, 6, 4) * 10
  )
})

test_that("velocity_unit_convert() converts from cm to cm", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/s",
      units_out = "cm/s"
    ),
    c(10, 6, 4)
  )
})

test_that("velocity_unit_convert() converts from cm to m", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/s",
      units_out = "m/s"
    ),
    c(10, 6, 4) / 100
  )
})

test_that("velocity_unit_convert() converts from cm to km", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/s",
      units_out = "km/s"
    ),
    c(10, 6, 4) / 100 / 1000
  )
})

test_that("velocity_unit_convert() converts from km to cm", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "km/s",
      units_out = "cm/s"
    ),
    c(10, 6, 4) * 100 * 1000
  )
})

test_that("velocity_unit_convert() converts from m to cm", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "m/s",
      units_out = "cm/s"
    ),
    c(10, 6, 4) * 100
  )
})

test_that("velocity_unit_convert() converts from mm to cm", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "mm/s",
      units_out = "cm/s"
    ),
    c(10, 6, 4) / 10
  )
})

# Testing for imperial units
test_that("velocity_unit_convert() converts from cm to in", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/s",
      units_out = "in/s"
    ),
    c(10, 6, 4) * 10 * (12 / 304.8)
  )
})

test_that("velocity_unit_convert() converts from cm to ft", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/s",
      units_out = "ft/s"
    ),
    c(10, 6, 4) * 10 * (1 / 304.8)
  )
})

test_that("velocity_unit_convert() converts from cm to yd", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/s",
      units_out = "yd/s"
    ),
    c(10, 6, 4) * 10 * (1 / 914.4)
  )
})

# Testing all times as both numerator and denominator
test_that("velocity_unit_convert() converts from s to s", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/s",
      units_out = "cm/s"
    ),
    c(10, 6, 4)
  )
})

test_that("velocity_unit_convert() converts from in to cm", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "in/s",
      units_out = "cm/s"
    ),
    c(10, 6, 4) * (304.8 / 12) * (1 / 10)
  )
})

test_that("velocity_unit_convert() converts from ft to cm", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "ft/s",
      units_out = "cm/s"
    ),
    c(10, 6, 4) * 304.8 * (1 / 10)
  )
})

test_that("velocity_unit_convert() converts from yd to cm", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "yd/s",
      units_out = "cm/s"
    ),
    c(10, 6, 4) * 304.8 * 3 * (1 / 10)
  )
})

test_that("velocity_unit_convert() converts from s to m", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/s",
      units_out = "cm/m"
    ),
    c(10, 6, 4) * 60
  )
})

test_that("velocity_unit_convert() converts from s to h", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/s",
      units_out = "cm/h"
    ),
    c(10, 6, 4) * 3600
  )
})

test_that("velocity_unit_convert() converts from m to s", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/m",
      units_out = "cm/s"
    ),
    c(10, 6, 4) / 60
  )
})

test_that("velocity_unit_convert() converts from m to h", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/m",
      units_out = "cm/h"
    ),
    c(10, 6, 4) * 60
  )
})

test_that("velocity_unit_convert() converts from h to h", {
  expect_equal(
    velocity_unit_convert(
      x = c(10, 6, 4),
      units_in = "cm/h",
      units_out = "cm/h"
    ),
    c(10, 6, 4)
  )
})
