test_that("output_given_with_no_dir", {
  dir <- "urbanr_data"
  if (dir.exists(dir)) unlink(dir, recursive=TRUE)
  expect_false(dir.exists(dir))
  expect_message(download_or_check_impervious(), "Data directory created at")
  expect_true(dir.exists(dir))
  unlink(dir, recursive=TRUE)
})

test_that("urbanr_dir_created", {
  download_or_check_impervious()
  testthat::expect_true(dir.exists("urbanr_data"))
  # Cleanup
  if (dir.exists("urbanr_data"))
    unlink("urbanr_data", recursive = TRUE)
})

test_that("finds_urbanr_dir", {
  download_or_check_impervious()
  expect_message(download_or_check_impervious())
  # Cleanup
  if (dir.exists("urbanr_data"))
    unlink("urbanr_data", recursive = TRUE)
})

test_that("urbanr_data_is_not_detected", {
  expect_false(download_or_check_impervious(return_status = TRUE))
  # Cleanup
  if (dir.exists("urbanr_data"))
    unlink("urbanr_data", recursive = TRUE)
})
