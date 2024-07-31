# Unit tests for the list_tzones() function.

require("dplyr")

# Note: this is essentially tautological
test_that("list_tzones() works", {
  # Getting required packages
  expect_equal(
    list_tzones(),
    timezones_df |>
      dplyr::select(-assume, -override, -zone)
  )
})
