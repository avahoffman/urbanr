#' Title
#'
#' @param relative_dir
#' @param filename
#'
#' @returns
#' @export
#'
#' @examples
download_or_check <- function(relative_dir, filename) {
  data_dir <- paste0("urbanr_data/", relative_dir)
  file_dir <- paste0(data_dir, "/", filename)
  url <- paste0("https://raw.githubusercontent.com/avahoffman/urbanr_data/main/", relative_dir, "/", filename)
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = T)
  }
  if (!file.exists(file_dir)){
    # TODO error for bad url. error for no internet connection
    download.file(url, file_dir)
  }
}


get_pct_impervious <- function(latlon){
  relative_dir <- "NLCD_2016_Impervious"
  filename <- "NLCD_2016_Impervious_L48_20190405.img"

  # Get the necessary data
  download_or_check(relative_dir, filename)
  data_source <- paste0(here::here(), "/urbanr_data/", relative_dir, "/", filename)
  r <- terra::rast(data_source)
  the_crs <- terra::crs(r)

  if(is.numeric(latlon)){
    # TODO: Checks for realistic ranges for values so as not to swap lat and long
    vals <- data.frame(lon = latlon[2], lat = latlon[1])
    vals_terra <- terra::vect(vals, crs = "+proj=longlat", keepgeom=T)
    extracted_vals <- terra::extract(r, terra::project(vals_terra, the_crs))
  }

}


  latlon <- c(39.640019, -76.756564)
  # Possible to download files directly from figshare: eg download.file("https://figshare.com/ndownloader/files/12919973", "downloaded.csv")
