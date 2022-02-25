.onAttach <- function(libname, pkgname) {
  if (interactive()) {
   if (!has_data_path()){
     packageStartupMessage("path to data has not been configured - please see ?set_data_path")
   }
  }
}