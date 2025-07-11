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
  if (!file.exists(file_dir)){
    cli::cli_alert(cli::col_cyan(
      "Data not found! Please first download the data from ",
      cli::style_hyperlink("National Land Cover Database (NLCD)", "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/Annual_NLCD_FctImp_2024_CU_C1V1.zip"),
      ". You can search for more snapshots ",
      cli::style_hyperlink("here", "https://www.mrlc.gov/data?f%5B0%5D=category%3AFractional%20Impervious%20Surface"),
      "."
    ))
    cli::cli_alert(cli::col_cyan(
      "Next, add the .tif file to your working directory urbanr_data folder (",
      paste0(getwd(), "/urbanr_data"),
      ")."
    ))
  }
}


#' Title
#'
#' @param latlon
#' @param edition
#'
#' @returns
#' @export
#'
#' @examples
get_pct_impervious <- function(latlon, edition = "Annual_NLCD_FctImp_2024_CU_C1V1.tif"){
  data_dir <- paste0("urbanr_data")
  file_dir <- paste0(data_dir, "/", edition)
  # Get the necessary data
  # TODO Error out if not correct dataset
  download_or_check_impervious(edition = edition)
  r <- terra::rast(file_dir)
  the_crs <- terra::crs(r)

  if(is.numeric(latlon)){
    # TODO: Checks for realistic ranges for values so as not to swap lat and long
    vals <- data.frame(lon = latlon[2], lat = latlon[1])
    vals_terra <- terra::vect(vals, crs = "+proj=longlat", keepgeom=T)
    extracted_vals <- terra::extract(r, terra::project(vals_terra, the_crs))
    #TODO nicer output
  }

  return(extracted_vals)
}

download_or_check_impervious()
get_pct_impervious(c(39.458686, -76.635277), edition = "Annual_NLCD_FctImp_2024_CU_C1V1.tif")
