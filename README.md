# unpakathon
Data snapshot of unpak data with supporting functions and vignettes.

Make sure to install these packages: devtools, dplyr, ggplot2, igraph

This package depends on  'adjustPhenotypes'. You can install from github using the incomparable devtools package:

devtools::install_github("stranda/adjustPhenotypes")

After installing adjustPhenotypes, you can install this package:

devtools::install_github("stranda/unpakathon",force=T,build_vignettes=T)

The 'build_vignettes' option makes it slower to install, but it provides the ability to say:

browseVignettes("unpakathon")

at the R prompt and choose among several tutorials on this data project