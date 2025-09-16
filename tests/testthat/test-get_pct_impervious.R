test_that("dataframe_with_several_cols_ok", {
  # create data dir
  dir.create("urbanr_data")
  
  ## Compare
  expect_equal(
    get_pct_impervious(
      data.frame(
        lat = 39.458686,
        lon = -76.635277,
        extra = 100,
        extra2 = "hello"
      ),
      edition = "test_2024.tif",
      data_dir = "fixtures"
    ),
    get_pct_impervious(
      data.frame(lat = 39.458686, lon = -76.635277),
      edition = "test_2024.tif",
      data_dir = "fixtures"
    )
  )
  
  # Cleanup
  if (dir.exists("urbanr_data"))
    unlink("urbanr_data", recursive = TRUE)
})

test_that("different_editions_different_colnames", {
  # create data dir
  dir.create("urbanr_data")
  
  ## Compare
  expect_equal(colnames(
    get_pct_impervious(
      data.frame(lat = 39.458686, lon = -76.635277),
      edition = "test_1988.tif",
      data_dir = "fixtures"
    )
  )[3],
  "Annual_NLCD_FctImp_1988_CU_C1V1")
  expect_equal(colnames(
    get_pct_impervious(
      data.frame(lat = 39.458686, lon = -76.635277),
      edition = "test_2024.tif",
      data_dir = "fixtures"
    )
  )[3],
  "Annual_NLCD_FctImp_2024_CU_C1V1")
  
  # Cleanup
  if (dir.exists("urbanr_data"))
    unlink("urbanr_data", recursive = TRUE)
})
