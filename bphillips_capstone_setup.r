###############################################################################
#> bphillips_capstone_setup
#> Data ingestion and libraries
###############################################################################

library(haven)
library(car)
library(broom)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(mgcv)
library(splines)
library(caret)
library(glmnet)
library(pROC)

# Reading data files
# d_sas <- read_sas("S:/HMG-Capstone-Phillips/Data/dedup_pt_encounters_eci_analysis.sas7bdat")
d_sas <- read.csv("capstone_data.csv")
