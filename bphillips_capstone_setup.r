###############################################################################
#> bphillips_capstone_setup
#> Data ingestion and libraries
###############################################################################

library(tidyverse)
library(haven)
library(car)
library(broom)
library(ggplot2)

# Reading data files
# setwd("S:/HMG-Capstone-Phillips/Data")
d_sas <- read_sas("S:/HMG-Capstone-Phillips/Data/dedup_pt_encounters_eci_analysis.sas7bdat")