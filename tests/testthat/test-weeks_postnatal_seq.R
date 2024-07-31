# Unit tests for the weeks_postnatal_seq() function.


pnw <- structure(c(
  `Postnatal Week 0` = 19742, `Postnatal Week 1` = 19749,
  `Postnatal Week 2` = 19756, `Postnatal Week 3` = 19763, `Postnatal Week 4` = 19770,
  `Postnatal Week 5` = 19777, `Postnatal Week 6` = 19784, `Postnatal Week 7` = 19791,
  `Postnatal Week 8` = 19798, `Postnatal Week 9` = 19805, `Postnatal Week 10` = 19812,
  `Postnatal Week 11` = 19819, `Postnatal Week 12` = 19826, `Postnatal Week 13` = 19833,
  `Postnatal Week 14` = 19840, `Postnatal Week 15` = 19847, `Postnatal Week 16` = 19854
), class = "Date")

test_that("weeks_postnatal_seq() returns expected results with as.Date() input for dob", {
  expect_equal(
    weeks_postnatal_seq(
      dob = as.Date("2024-01-20"),
      n_weeks = 16
    ),
    pnw
  )
})

test_that("weeks_postnatal_seq() returns expected results with character input for dob when in quiet", {
  expect_equal(
    weeks_postnatal_seq(
      dob = "2024-01-20",
      n_weeks = 16,
      quiet = TRUE
    ),
    pnw
  )
})

test_that("weeks_postnatal_seq() throws a warning for character vector", {
  expect_warning(
    weeks_postnatal_seq(
      dob = "2024-01-20",
      n_weeks = 16
    ),
    "dob coerced from character to Date type"
  )
})

test_that("weeks_postnatal_seq() throws an error for character vector that cannot be coerced to a Date", {
  expect_error(
    weeks_postnatal_seq(
      dob = "Hi there.",
      n_weeks = 16
    ),
    "dob cannot be coerced to a Date"
  )
})

test_that("weeks_postnatal_seq() throws an error when dob is neither Date nor character", {
  expect_error(
    weeks_postnatal_seq(
      dob = FALSE,
      n_weeks = 16
    ),
    "dob is neither a Date nor a character vector"
  )
})
