
# urbanr

<!-- badges: start -->
<!-- badges: end -->

The goal of urbanr is to gather data related to an urban sciences study.

## Installation

You can install the development version of urbanr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("avahoffman/urbanr")
```

You will need to download data from [NLCD](https://www.mrlc.gov/data?f%5B0%5D=category%3AFractional%20Impervious%20Surface/) or from our backup version on [Figshare](https://figshare.com/articles/dataset/urbanr_data_Annual_NLCD_FctImp_2024/29549666?file=56194733).

## Example

This is a basic example to get percent impervious values from latitude and longitude coordinates. This function will also give you instructions if data is missing.

``` r
library(urbanr)

# Single point query
get_pct_impervious(c(39.458686, -76.635277))

# Batch query with multiple points
coords_df <- data.frame(
  lat = c(39.458686, 39.333241),
  lon = c(-76.635277, -76.587142)
)
get_pct_impervious(coords_df)
```

