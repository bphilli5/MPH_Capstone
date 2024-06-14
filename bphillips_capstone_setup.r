###############################################################################
#> bphillips_capstone_setup
#> Data ingestion and libraries
###############################################################################

library(tidyverse)
library(haven)
library(car)
library(broom)

# Reading data files
# setwd("S:/HMG-Capstone-Phillips/Data")
# d_sas <- read_sas("dedup_pt_encounters_eci_analysis.sas7bdat")
# 
# setwd("C:/Users/brend/OneDrive/Documents")
# write.csv(d_sas, file = "capstone_data.csv", row.names = FALSE)
d_csv <- read.csv("capstone_data.csv")
