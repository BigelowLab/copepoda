
#' Retrieve a named vector of column names for copepod shortform
#'
#' @export
#' @param strip logical, if TRUE strip the names off
#' @return named vector of column names
copepod_shortform_names <- function(strip = TRUE){
  
  x  = c(
    "SHP-CRUISE"                  = "shp_cruise",
    "YEAR"                        = "year",
    "MON"                         = "month",
    "DAY"                         = "day",
    "TIMEgmt"                     = "timegmt",
    "TIMEloc"                     = "timeloc",
    "LATITUDE"                    = "lat",
    "LONGITDE"                    = "lon",
    "UPPER_Z"                     = "zupper",
    "LOWER_Z"                     = "zlower",
    "T"                           = "towtype",
    "GEAR"                        = "gear",
    "MESH"                        = "mesh",
    "NMFS_PGC"                    = "nmfs_pgx",
    "ITIS_TSN"                    = "itis_tsn",
    "MOD"                         = "mod",
    "LIF"                         = "lifestage_name",
    "PSC"                         = "lifestage",
    "SEX"                         = "sex",
    "V"                           = "value_type",
    "Water Strained"              = "vol_water_strained",
    "Original-VALUE"              = "original_value",
    "Orig-UNITS"                  = "original_units",
    "VALUE-per-volu"              = "value_per_vol",
    "UNITS"                       = "value_per_vol_units",
    "F1"                          = "global_ann_range_per_vol",
    "F2"                          = "basin_ann_range_per_vol",
    "F3"                          = "basin_ssn_range_per_vol",
    "F4"                          = "basin_mon_range_per_vol",
    "VALUE-per-area"              = "value_per_area",
    "UNITS"                       = "value_per_area_units",
    "F1"                          = "global_ann_range_per_area",
    "F2"                          = "basin_ann_range_per_area",
    "F3"                          = "basin_ssn_range_per_area",
    "F4"                          = "basin_mon_range_per_area",
    "SCIENTIFIC NAME -[ modifiers ]-" = "sciname_modifiers",
    "RECORD-ID"                   = "record_id",
    "DATASET-ID"                  = "dataset_id",
    "SHIP"                        = "ship",
    "PROJ"                        = "project",
    "INST"                        = "instituion",
    "Orig-CRUISE-ID"              = "orig_cruise_id",
    "Orig-STATION-ID"             = "orig_station_id",
    "Taxa-Name"                   = "taxa_name",
    "Taxa-Modifiers"              = "taxa_mod",
    "Dummy"                       = "dummy")
  if (strip) x = unname(x)
  x
}

#' Read a short-form data file
#'
#' @param filename the name of the file
#' @param skip the number of header lines to skip
#' @param col_names a vector of columns names including a trailing 'dummy' which is dropped
#' @return tibble
read_copepod_shortform <- function(filename,
                                   skip = 17, 
                                   col_names = copepod_shortform_names()){
  readr::read_csv(filename, skip = skip, col_names = col_names, show_col_types = FALSE) |>
    dplyr::select(-.data$dummy)
}

#' Read a copepod short form - trim to bare necessities
#'
#' @export
#' @param filename one or more filenames.  If multiples, the
#'   individual tables are row bound into one.
#' @param simplify logical if TRUE trim down to a simple dataset
#' @param select_vars character, the columns to select if simplyfing.  Ignored  
#'   unless \code{simplify = TRUE}. 
#' @param form character either 'tibble' or 'sf'
#' @return tibble or sf Points object
read_copepod <- function(filename = list_data(name = 'us-05101'),
                         simplify = TRUE,
                         select_vars = c("date", "lon", "lat",
                               "zupper", "zlower", "value_per_vol", 
                               "value_per_area", "lifestage"),
                         form = c("tibble", "sf")[1]){
  
  x <- lapply(filename, read_copepod_shortform) |>
    dplyr::bind_rows()
  
  if (simplify) {
    x <- x |>
      dplyr::mutate(
         date = as.POSIXct(paste(x$year, x$month, x$day, "00:00:00"),
                        format = '%Y %m %d %H:%M:%S', tz = "UTC") + x$timegmt/24.0,
        .before = 1) |>
      dplyr::select(dplyr::all_of(select_vars))
  }
  if (tolower(form[1]) == 'sf'){
    x <- sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
  }
  x
}
