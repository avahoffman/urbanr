#' Check existence of impervious surface raster data
#'
#' This function helps the user find the appropriate data from NLCD impervious surface raster data.
#' It creates the necessary directory structure and provides guidance if the data is missing.
#'
#' @details
#'
#' 1. Data should be downloaded from the [USGS NLCD](https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/Annual_NLCD_FctImp_2024_CU_C1V1.zip).
#' Data files are quite large (~1GB).
#'
#' 2. Next, the zip file should be unpacked and the .tif file should be placed in the `urbanr_data` directory inside the working directory.
#'
#' Alternatively, you can download a different snapshot from the NLCD
#' [here](https://www.mrlc.gov/data?f%5B0%5D=category%3AFractional%20Impervious%20Surface) and change the
#' `edition` argument to that .tif filename. File should still be placed in the proper
#' directory.
#'
#' In the event that the data is no longer available from NLCD, the 2024 snapshot
#' can also be found on [Figshare](https://figshare.com/articles/dataset/urbanr_data_Annual_NLCD_FctImp_2024/29549666?file=56194733).
#'
#' @param edition Character string specifying the NLCD data edition to use. Defaults to
#' "Annual_NLCD_FctImp_2024_CU_C1V1.tif".
#' @param return_status Boolean where `FALSE` returns nothing, and `TRUE` returns the staus of the data. `TRUE`
#' is returned if appropriate data is found.
#'
#' @importFrom cli cli_alert col_cyan style_hyperlink
#'
#' @returns Function produces no output if ready to proceed; produces messaging if data is not available to the user.
#'
#' @examples
#' download_or_check_impervious()
#' @export
download_or_check_impervious <- function(edition = "Annual_NLCD_FctImp_2024_CU_C1V1.tif",
                                         return_status = FALSE) {
  # Set up the directory structure
  data_dir <- paste0("urbanr_data")
  file_dir <- paste0(data_dir, "/", edition)
  
  # Create the directory if it doesn't exist
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = T)
    cli::cli_bullets(c("v" = cli::col_cyan("Data directory created at ", getwd())))
  }
  
  # Check if the file exists and provide instructions if missing
  if (!file.exists(file_dir)) {
    cli::cli_bullets(c(
      "x" =
        cli::col_cyan(
          "Data not found! Please first download the data from ",
          cli::style_hyperlink(
            "National Land Cover Database (NLCD)",
            "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/Annual_NLCD_FctImp_2024_CU_C1V1.zip"
          ),
          ". You can search for more snapshots ",
          cli::style_hyperlink(
            "here",
            "https://www.mrlc.gov/data?f%5B0%5D=category%3AFractional%20Impervious%20Surface"
          ),
          "."
        )
    ))
    cli::cli_bullets(c(
      "i" =
        cli::col_cyan(
          "Next, add the .tif file to your working directory urbanr_data folder (",
          paste0(getwd(), "/urbanr_data"),
          ")."
        )
    ))
    if (return_status)
      return(FALSE)
  } else {
    if (return_status)
      return(TRUE)
  }
}


#' Extract percentage impervious surface cover from NLCD data
#'
#' This function extracts impervious surface percentages from NLCD raster data for specific
#' geographic locations.
#'
#' @details
#' The function returns a data frame containing the coordinate pairs and their corresponding
#' impervious surface percentages. It returns a data frame with
#' multiple rows, each representing a location and its corresponding impervious surface percentage.
#'
#' @param latlon A data frame with columns named 'lat' and 'lon'. Coordinates should be
#'               in decimal degrees format.
#' @param edition Character string specifying the NLCD data edition to use. Defaults to
#'                "Annual_NLCD_FctImp_2024_CU_C1V1.tif".
#' @param data_dir Character string specifying the directory where the edition is stored. For testing.
#'
#' @importFrom cli cli_alert col_cyan
#' @importFrom terra rast vect extract crs project
#'
#' @returns A data frame containing coordinate pairs and their corresponding impervious surface
#'         percentages.
#'
#' @examplesIf download_or_check_impervious(return_status = TRUE)
#' # Batch query with multiple points
#' coords_df <- data.frame(
#'   lat = c(39.458686, 39.333241),
#'   lon = c(-76.635277, -76.587142)
#' )
#' get_pct_impervious(coords_df)
#' @export
get_pct_impervious <- function(latlon,
                               edition = "Annual_NLCD_FctImp_2024_CU_C1V1.tif",
                               data_dir = "urbanr_data") {
  #data_dir <- paste0("urbanr_data")
  file_dir <- paste0(data_dir, "/", edition)
  
  # Get the necessary data
  # TODO Error out if not correct dataset, or just look for a tif file
  download_or_check_impervious(edition = edition)
  cli::cli_bullets(c("v" = cli::col_cyan("Using data from: ", paste0(edition), ".")))
  
  r <- terra::rast(file_dir)
  the_crs <- terra::crs(r)
  
  # Confirm data is formatted correctly
  if (is.data.frame(latlon)) {
    # Confirm column names for terra
    if ("lat" %in% colnames(latlon) &
        "lon" %in% colnames(latlon)) {
      vals <- data.frame(lon = latlon$lon, lat = latlon$lat)
      vals_terra <- terra::vect(vals, crs = "+proj=longlat", keepgeom = T)
      extracted_vals <- terra::extract(r, terra::project(vals_terra, the_crs))
      out <- cbind(vals, extracted_vals[, 2])
      colnames(out)[3] <- colnames(extracted_vals[2])
      return(out)
    } else {
      lat_lon_dataframe_alert()
    }
  } else {
    lat_lon_dataframe_alert()
  }
}


#' Helper function to display alert for invalid latitude/longitude data frame
#'
#' @noRd
lat_lon_dataframe_alert <- function() {
  return(cli::cli_bullets(c(
    "x" = cli::col_cyan(
      "Please ensure the input format is a dataframe that contains two columns `lat` and `lon` corresponding to the latitude and longitude, respectively."
    )
  )))
}


make_test_data <- function() {
  file_dir <- "urbanr_data/Annual_NLCD_FctImp_2024_CU_C1V1.tif"
  r <- terra::rast(file_dir)
  the_crs <- terra::crs(r)
  limited_extent <- terra::ext(c(-76.635277, -76.635270, 39.458686, 39.458690))
  limited_extent <- terra::project(limited_extent, from = "+proj=longlat", to = the_crs)
  cropped_r <- terra::crop(r, limited_extent)
  
  terra::writeRaster(cropped_r,
                     filename = file.path("data/test_2024.tif"),
                     overwrite = TRUE)
  
  file_dir <- "urbanr_data/Annual_NLCD_FctImp_1988_CU_C1V1.tif"
  r <- terra::rast(file_dir)
  the_crs <- terra::crs(r)
  limited_extent <- terra::ext(c(-76.635277, -76.635270, 39.458686, 39.458690))
  limited_extent <- terra::project(limited_extent, from = "+proj=longlat", to = the_crs)
  cropped_r <- terra::crop(r, limited_extent)
  
  terra::writeRaster(cropped_r,
                     filename = file.path("data/test_1988.tif"),
                     overwrite = TRUE)
}
