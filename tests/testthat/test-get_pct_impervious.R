test_that("dataframe_with_several_cols_ok", {
  expect_equal(
    get_pct_impervious(
      data.frame(
        lat = 39.458686,
        lon = -76.635277,
        extra = 100,
        extra2 = "hello"
      ),
      edition = "test_2024.tif",
      data_dir = "tests/testthat/test_data"
    ),
  get_pct_impervious(
    data.frame(
      lat = 39.458686,
      lon = -76.635277
    ),
    edition = "test_2024.tif",
    data_dir = "tests/testthat/test_data"
  )
  )
})

test_that("different_editions_different_colnames", {
  expect_equal(
    colnames(get_pct_impervious(
      data.frame(
        lat = 39.458686,
        lon = -76.635277
      ),
      edition = "test_1988.tif",
      data_dir = "tests/testthat/test_data"
    ))[3],
    "Annual_NLCD_FctImp_1988_CU_C1V1"
  )
  expect_equal(
    colnames(get_pct_impervious(
      data.frame(
        lat = 39.458686,
        lon = -76.635277
      ),
      edition = "test_2024.tif",
      data_dir = "tests/testthat/test_data"
    ))[3],
    "Annual_NLCD_FctImp_2024_CU_C1V1"
  )
})
