# Unit tests for the list_tzones() function.

# Note: this is essentially tautological
test_that("list_tzones() works", {
  expect_equal(list_tzones(),
               timezones_df |>
                 dplyr::select(-assume, -override, -zone))
})
