###############################################################################
#> bphillips_capstone_setup
#> Data ingestion and libraries
###############################################################################

library(haven)
library(car)
library(broom)
library(ggplot2)
library(tidyverse)

# Reading data files
d_sas <- read_sas("S:/HMG-Capstone-Phillips/Data/dedup_pt_encounters_eci_analysis.sas7bdat")
