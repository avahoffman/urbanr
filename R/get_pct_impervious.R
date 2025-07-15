#' Title
#'
#' @param relative_dir
#' @param filename
#'
#' @returns
#' @export
#'
#' @examples
download_or_check_impervious <- function(edition = "Annual_NLCD_FctImp_2024_CU_C1V1.tif") {
  data_dir <- paste0("urbanr_data")
  file_dir <- paste0(data_dir, "/", edition)
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = T)
  }
  # Backup file https://figshare.com/articles/dataset/urbanr_data_Annual_NLCD_FctImp_2024/29549666?file=56194733
  if (!file.exists(file_dir)) {
    cli::cli_alert(
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
    )
    cli::cli_alert(
      cli::col_cyan(
        "Next, add the .tif file to your working directory urbanr_data folder (",
        paste0(getwd(), "/urbanr_data"),
        ")."
      )
    )
  }
}


#' Title
#'
#' @param latlon either a vector of length 2 containing numeric latitude, longitude coordinates OR a dataframe containing two columns with the colnames `lat` and `lon`.
#' @param edition
#'
#' @returns
#' @export
#'
#' @examples
#' get_pct_impervious(c(39.458686, -76.635277)) # should equal 89
#' df <- data.frame(lat = c(39.458686, 39.333241), lon = c(-76.635277, -76.587142))
#' get_pct_impervious(df) # should equal 89 and 9
get_pct_impervious <- function(latlon, edition = "Annual_NLCD_FctImp_2024_CU_C1V1.tif") {
  data_dir <- paste0("urbanr_data")
  file_dir <- paste0(data_dir, "/", edition)

  # Get the necessary data
  # TODO Error out if not correct dataset
  download_or_check_impervious(edition = edition)
  r <- terra::rast(file_dir)
  the_crs <- terra::crs(r)

  # Confirm data is formatted correctly
  if (is.numeric(latlon) & length(latlon) == 2) {
    # TODO: Checks for realistic ranges for values so as not to swap lat and long
    vals <- data.frame(lon = latlon[2], lat = latlon[1])
    vals_terra <- terra::vect(vals, crs = "+proj=longlat", keepgeom = T)
    extracted_vals <- terra::extract(r, terra::project(vals_terra, the_crs))
    out <- cbind(vals, extracted_vals[, 2])
    colnames(out)[3] <- colnames(extracted_vals[2])
    return(out)

  } else if (is.data.frame(latlon)) {
    if (ncol(latlon) == 2) {
      if ("lat" %in% colnames(latlon) & "lon" %in% colnames(latlon)) {
        vals <- latlon
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

  } else {
    cli::cli_alert(
      cli::col_cyan(
        "Input must be either a vector of length 2 e.g., `c(39.458686, -76.635277)` OR a dataframe containing two columns with the colnames `lat` and `lon`."
      )
    )
  }
}


lat_lon_dataframe_alert <- function() {
  return(cli::cli_alert(
    cli::col_cyan(
      "Please ensure the dataframe contains only two columns `lat` and `lon` corresponding to the latitude and longitude, respectively."
    )
  ))
}
