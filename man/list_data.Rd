% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path.R
\name{list_data}
\alias{list_data}
\title{Retrieve the path to one or more copepod datasets}
\usage{
list_data(
  copepid = "us-04201",
  form = c("short", "full")[1],
  path = get_data_path()
)
}
\arguments{
\item{copepid}{character, the copepid of the data set ala "us-04201"}

\item{form}{character, one of "short" or "full"}

\item{path}{character the root path}
}
\value{
character vector of filenames
}
\description{
Retrieve the path to one or more copepod datasets
}
