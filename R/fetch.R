#' Retrieve a NMFS url, possible to a downloadable dataset.
#' 
#' @export
#' @param ... file path segments for use with \code{\link[base]{file.path}}
#' @param copepid NA or char, if not NA then assumed to be a dataset copepid like 'us-04210' 
#'   or the like.  If not NA then \code{...} arguments are ignored.
#' @param base_url char, the root path to NMFS datasets
#' @return charcater URL
copepod_url <- function(...,
                     copepid = c(NA, "us-04201")[1], 
                     base_url = "https://www.st.nmfs.noaa.gov/copepod/data"){
  
  # https://www.st.nmfs.noaa.gov/copepod/data/us-04201/copepod__us-04201.zip
  if (is.na(copepid[1])){
    u <- file.path(base_url, ...)
  } else {
    u <- file.path(base_url, sprintf("%s/copepod__%s.zip", copepid[1], copepid[1]))
  }
  u
}

#' Fecth COPEPD data from NMFS
#' 
#' @export
#' @param copepid character, the copepid of the data set ala "us-04201"
#' @param dest character, the path to save the data to
#' @param decompress logical, if TRUE unpack the downloaded zip file
#' @param cleanup logical, if TRUE remove the downloaded zip file. Ignored unless
#'   \code{decompress} is \code{TRUE}
#' @return the result of \code{\link[utils]{download.file}}
fetch_copepod <- function(copepid = "us-04201",
                          dest = get_data_path(),
                          decompress = TRUE,
                          cleanup = decompress){
  
  u <- copepod_url(copepid = copepid)
  destfile <- file.path(dest, basename(u))
  ok <- try(download.file(u, destfile, mode = 'wb'))
  if (inherits(ok, 'try-error') || ok > 0){
    warning("unable to download copeid:", copepid)
    return(ok)
  }
  if (decompress){
    filelist <- unzip(destfile, exdir = dest, overwrite = TRUE)
    if (cleanup) unlink(destfile)
  }
  ok
}

