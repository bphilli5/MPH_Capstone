###############################################################################
#> bphillips_capstone_setup
#> Data ingestion and libraries
###############################################################################

library(tidyverse)
library(haven)
library(car)

# Reading data files
setwd("S:/HMG-Capstone-Phillips/Data")
d_sas <- read_sas("dedup_pt_encounters_eci_analysis.sas7bdat")
