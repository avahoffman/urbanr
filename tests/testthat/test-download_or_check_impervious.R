test_that("output_given_with_no_dir", {
  expect_message(download_or_check_impervious())
  # Cleanup
  if (dir.exists("urbanr_data"))
    unlink("urbanr_data", recursive = TRUE)
})

test_that("urbanr_dir_created", {
  download_or_check_impervious()
  testthat::expect_true(dir.exists("urbanr_data"))
  # Cleanup
  if (dir.exists("urbanr_data"))
    unlink("urbanr_data", recursive = TRUE)
})

test_that("urbanr_output_given_with_dir", {
  download_or_check_impervious()
  expect_message(download_or_check_impervious())
  # Cleanup
  if (dir.exists("urbanr_data"))
    unlink("urbanr_data", recursive = TRUE)
})
