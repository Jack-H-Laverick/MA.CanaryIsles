
## Copy StrathE2E parameter files so that the implementation document can pull values

library(ggplot2) ; source("./R scripts/@_Region file.R")  # ggplot is needed to source the Region file

R.utils::copyDirectory(stringr::str_glue("./StrathE2E/"),
                       "./Implementation doc/Files/")     # Copy files to pull parameters from

file.copy("./Objects/Domains.rds", "./Implementation doc/Domains.rds", overwrite = TRUE) # Copy domain file