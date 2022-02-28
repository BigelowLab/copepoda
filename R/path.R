#' Test if a data path config file exists
#' 
#' @export
#' @param filename char, the name of the config files
#' @return logical TRUE if the config file exists
has_data_path <- function(filename = "~/.copepoda"){
  file.exists(filename[1])
}

#' Write the configuration file
#' 
#' @export
#' @param path char, the path to the data - like '/mnt/ecocast/coredata/noaa/nmfs/copepod'
#' @param filename char, the name of the config files
set_data_path <- function(path, filename = "~/.copepoda"){
  cat(path[1], sep = "\n", file = filename[1])
}

#' Retrieve the user specified data path
#'
#' @export
#' @param ... elements for \code{\link[base]{file.path}}
#' @param root char, the root path directory
#' @return character, the path to the data
get_data_path <- function(...,
                          root = readLines("~/.copepoda")){
  file.path(root, ...)
}

#' Retrieve the path to one or more copepod datasets
#'
#' @export
#' @param copepid character, the copepid of the data set ala "us-04201"
#' @param form character, one of "short" or "full"
#' @param path character the root path 
#' @return character vector of filenames
list_data <- function(copepid = "us-04201",
                      form = c("short", "full")[1],
                      path = get_data_path()){
                      
  if (tolower(form[1]) == "short"){
    pattern = glob2rx(sprintf("*_%s.csv", copepid[1]))
    form_segment <- "short-format"
  } else {
    pattern = glob2rx(sprintf("*_%s.txt", copepid[1]))
    form_segment <- "full-format"
  }
  
  path <- file.path(path,
                       sprintf("copepod__%s", copepid[1]),
                       "data_src",
                       form_segment)
  list.files(path,
             pattern = pattern,
             full.names = TRUE,
             recursive = TRUE)
}