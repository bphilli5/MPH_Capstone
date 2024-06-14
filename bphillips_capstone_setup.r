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

setwd("C:/Users/brend/OneDrive/Documents")
write.csv(d_sas, file = "capstone_data.csv", row.names = FALSE)
d_csv <- read.csv("capstone_data.csv")

d_tables <- d_csv %>% 
  mutate(across(c(ed_30days,
                  readmission_30days),
                ~recode(.,"N"="No","Y"="Yes",.default=NULL),
                .names="{.col}_recode"),
         sex_recode = ifelse(sex == "", "Unknown", sex),
         ethnicity_recode = case_when(
           ethnicity == "Hispanic, Latino/a, or Spanish Origin" ~ "Hispanic",
           ethnicity == "Non-Hispanic, Latino/a, or Spanish Origin" ~ "Hispanic",
           ethnicity == "Patient Unable to Answer" ~ "Unknown",
           ethnicity == "*Unspecified" ~ "Unknown",
           ethnicity == "" ~ "Unknown",
           TRUE ~ ethnicity),
         ICU_days_recode = ifelse(ICU_days == "","No", "Yes"))
