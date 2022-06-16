# _targets.R file
library(targets)
source("R/functions.R")
tar_option_set(packages = c("readr", "tidyverse", "ggplot2", "sf", "tigris", "lubridate", "spdep", "tidycensus"))
list(
  tar_target(data, read_data()),
  tar_target(subdata, sub_data(data))
  
)

