#> Capstone Project
#> Brendan Phillips

library(tidyverse)
library(haven)

# Reading data files
setwd("S:/HMG-Capstone-Phillips/Data")
d <- read_sas("dedup_pt_encounters_eci_analysis.sas7bdat")
