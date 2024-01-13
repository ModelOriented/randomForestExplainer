test_that("functions work as expected without warnings", {

  expect_equal(min_na(NA), NA)
  expect_equal(max_na(NA), NA)
})
