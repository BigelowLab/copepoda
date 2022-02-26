Copepoda
================

An R package to locally serve copepod data from [NOAA NMFS
COPEPOD](https://www.st.nmfs.noaa.gov/copepod/) project. Thsi package
assists in the download of online datasets and simplifies local
management.

Many of the data values are
[coded](https://www.st.nmfs.noaa.gov/copepod/codes/), so it is important
the user has a working familiarity with the data source.

## Requirements

-   [R v4.1+](https://www.r-project.org/)

-   [rlang](https://CRAN.R-project.org/package=rlang)

-   [dplyr](https://CRAN.R-project.org/package=dplyr)

-   [readr](https://CRAN.R-project.org/package=readr)

-   [sf](https://CRAN.R-project.org/package=sf)

## Installation

    remotes::install_github("BigelowLab/copepoda")

## Initial Use

The premise of this package is that data may be stored in a single
location but accessed by many users. To achieve this resource-friendly
goal and still simplified access for each user, we need to inform the
package where the data resides. We do this by storing the path to the
data location in each user’s home directory in a hidden text file,
“\~/.copepoda”. That text file has just one line in it which contains
the full path to the shared dataset. For example, the author’s contains
`/mnt/ecocast/coredata/noaa/nmfs/copepod` which points to a shared
network drive mounted on our linux platform.

When the package is first loaded (ala `library(copepoda)`) the existence
of the file is check, and if missing a warning is issued.

You can create and populate that `~/.copepoda` using a text editors, or
you can create using the provided function `set_data_path()`. Here is
how the author created his own.

    library(copepoda)
    copepoda::set_data_path("/mnt/ecocast/coredata/noaa/nmfs/copepod")

That’s it. If you even move the data you’ll have to modify the contents
of this hidden text file.

## Fetching new data
