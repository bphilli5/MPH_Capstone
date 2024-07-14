###############################################################################
#> bphillips_capstone_tables
#> Creating descriptive statistics tables
###############################################################################

#> Step 1: Categorical variable recoding + variable selection 
#> Step 2: Setting up the sorting of the tables
#> Step 3: Creating the categorical variable table
#> Step 4: Creating numeric variable table
#> Step 5: Overall figures and payor stata counts

###############################################################################
# Step 1: Categorical variable recoding + variable selection 
###############################################################################

d_1 <- d_sas %>% 
  mutate(across(c(ed_30days,
                  readmission_30days),
                ~case_when(.=="N" ~ "No",
                           .=="Y" ~ "Yes",
                           .default=NULL),
                .names="{.col}_recode"),
         sex_recode = ifelse(sex == "" | sex == "Unknown", "Missing", sex),
         ethnicity_recode = case_when(
           ethnicity == "Hispanic, Latino/a, or Spanish Origin" ~ "Hispanic",
           ethnicity == "Non-Hispanic, Latino/a, or Spanish Origin" ~ "Non-Hispanic",
           ethnicity == "Patient Unable to Answer" ~ "Missing",
           ethnicity == "*Unspecified" ~ "Missing",
           ethnicity == "Unknown" ~ "Missing",
           ethnicity == "" ~ "Missing",
           TRUE ~ ethnicity),
         payor_category = case_when(
           payor_category == "Indigent Care" ~ "Other",
           TRUE ~ payor_category),
         ) %>% 
  
  select(
    # Outcomes
    readmission_30days_recode,
    ed_30days_recode,
    death_30_days,
    # Scores
    discharge_HOSPITAL_score,
    discharge_news_score,
    admission_HOSPITAL_score,
    admission_news_score,
    day_minus_2_HOSPITAL_score,
    day_minus_1_HOSPITAL_score,
    day_minus_1_news_score,
    day_minus_2_news_score,
    average_HOSPITAL_score,
    average_NEWS_score,
    # Categorical predictors
    payor_category,
    sex_recode,
    race_category,
    ethnicity_recode,
    language_category,
    ICU_category,
    dc_disp_category,
    discharge_service,
    patient_class,
    facility_name,
    # Numeric predictors
    age_at_encounter,
    los_in_hours,
    # Processed CMR indices
    CMR_Index_Readmission,
    CMR_Index_Mortality
  ) %>% 
  
  filter(
    age_at_encounter < 110
  )

d_tables <- d_1 %>% 
  # Process CMR indices
  mutate(CMR_Index_Readmission_Zero = if_else(CMR_Index_Readmission == 0,
                                              "Yes",
                                              "No"),
         CMR_Index_Readmission_NonZero = if_else(CMR_Index_Readmission > 0, 
                                                 CMR_Index_Readmission, 
                                                 NA_real_),
         CMR_Index_Mortality_Zero = if_else(CMR_Index_Mortality == 0,
                                            "Yes",
                                            "No"),
         CMR_Index_Mortality_NonZero = if_else(CMR_Index_Mortality > 0, 
                                               CMR_Index_Mortality, 
                                               NA_real_)
  )
###############################################################################
# Step 2: Setting up the sorting of the tables
###############################################################################

variables_ordered <- c("readmission_30days_recode",
                       "ed_30days_recode",
                       "death_30_days",
                       
                       "discharge_HOSPITAL_score",
                       "day_minus_1_HOSPITAL_score",
                       "day_minus_2_HOSPITAL_score",
                       "admission_HOSPITAL_score",
                       "discharge_news_score",
                       "day_minus_1_news_score",
                       "day_minus_2_news_score",
                       "admission_news_score",
                       
                       "age_at_encounter",
                       "sex_recode",
                       "race_category",
                       "ethnicity_recode",
                       "language_category",
                       "ICU_category",
                       "dc_disp_category",
                       "discharge_service",
                       "patient_class",
                       "facility_name",
                       "los_in_hours",
                       "CMR_Index_Readmission_Zero",
                       "CMR_Index_Readmission_NonZero",
                       "CMR_Index_Mortality_Zero",
                       "CMR_Index_Mortality_NonZero"
)

table_value_order <- c("Yes",
                       "No",
                       
                       "Home",
                       "Post-acute care",
                       "AMA",
                       "Hospice",
                       
                       "Hispanic",     
                       "Non-Hispanic",
                       
                       "English", 
                       "Spanish",                                  
                       
                       "White or Caucasian",                        
                       "Black or African American",                 
                       "Asian",                                     
                       
                       "American Indian or Alaska Native",  
                       "Native Hawaiian and Other Pacific Islander",
                       "More than one race",
                       
                       "Male",                                    
                       "Female",
                       
                       "Medicine",
                       "Orthopedics",
                       "Surgery",
                       
                       "Other",
                       "Unknown",
                       "Missing")

###############################################################################
# Step 3: Creating the categorical variable table
###############################################################################

categorical_table <- d_tables %>% 
  select(readmission_30days_recode,
         ed_30days_recode,
         death_30_days,
         payor_category,
         sex_recode,
         race_category,
         ethnicity_recode,
         language_category,
         ICU_category,
         dc_disp_category,
         discharge_service,
         patient_class,
         facility_name,
         CMR_Index_Readmission_Zero,
         CMR_Index_Mortality_Zero) %>%
  
  pivot_longer(-payor_category, names_to = "variable", values_to = "value") %>% 
  group_by(payor_category, variable, value) %>% 
  summarise(count=n()) %>% 
  group_by(payor_category, variable) %>%
  mutate(
    total = sum(count),
    percentage = round(count / total * 100, 1),
    result = paste0(count, " (", percentage, "%)")) %>%
  select(-count, -total, -percentage) %>%
  pivot_wider(names_from = payor_category, 
              values_from = result, values_fill = "0 (0%)") %>%
  arrange(match(variable, variables_ordered), 
          match(value, table_value_order))

###############################################################################
# Step 4: Creating the numeric variable table
###############################################################################

numeric_table <- d_tables %>% 
  select(payor_category,
         age_at_encounter,
         admission_HOSPITAL_score,
         admission_news_score,
         day_minus_2_HOSPITAL_score,
         day_minus_1_HOSPITAL_score,
         day_minus_1_news_score,
         day_minus_2_news_score,
         discharge_HOSPITAL_score,
         discharge_news_score,
         los_in_hours,
         CMR_Index_Readmission_NonZero,
         CMR_Index_Mortality_NonZero) %>% 
  pivot_longer(-payor_category, names_to = "variable", values_to = "value") %>% 
  group_by(payor_category, variable) %>% 
  summarise(
    med = round(median(value, na.rm = TRUE), 1),
    q25 = round(quantile(value, 0.25, na.rm = TRUE), 1),
    q75 = round(quantile(value, 0.75, na.rm = TRUE), 1)
  ) %>% 
  mutate(result = paste0(med, " (", q25, "-", q75, ")")) %>% 
  select(-med, -q25, -q75) %>% 
  pivot_wider(names_from = payor_category, 
              values_from = result, values_fill = "N/A")

###############################################################################
# Step 5: Overall figures and payor stata counts
###############################################################################
# Payor strata counts
payor_cats <- unique(d_tables$payor_category)
payor_counts <- lapply(payor_cats, function(cat) {
  sum(d_tables$payor_category==cat)
})
catsncounts <- cbind(payor_cats,payor_counts)

var_names <- names(d_tables)
overall_figures <- list()
for (var in var_names) {
  if (is.numeric(d_tables[[var]])) {
    med <- round(median(d_tables[[var]], na.rm=TRUE), 2)
    q25 <- round(quantile(d_tables[[var]], 0.25, na.rm=TRUE), 2)
    q75 <- round(quantile(d_tables[[var]], 0.75, na.rm=TRUE), 2)
    overall_figures[[var]] <- paste0(med, " (", q25, "-", q75, ")")
  }
  else if (is.character(d_tables[[var]])) {
    names <- unique(d_tables[[var]])
    overall_figures[[var]] <- character(length(names))  # Initialize vector
    for (i in seq_along(names)) {
      name <- names[i]
      count <- sum(d_tables[[var]] == name, na.rm=TRUE)
      pct <- round(count / length(d_tables[[var]]) * 100, 1)
      overall_figures[[var]][i] <- paste0(count, " (", pct, "%)")
    }
    names(overall_figures[[var]]) <- names  # Assign names to the vector
  }
}

combined_table <- rbind(categorical_table,numeric_table) %>% 
  arrange(match(variable, variables_ordered), 
          match(value, table_value_order))

combined_table["Overall"] <- c(
  overall_figures$readmission_30days_recode['Yes'],
  overall_figures$readmission_30days_recode['No'],
  overall_figures$ed_30days_recode['Yes'],
  overall_figures$ed_30days_recode['No'],
  overall_figures$death_30_days['Yes'],
  overall_figures$death_30_days['No'],
  overall_figures$discharge_HOSPITAL_score,
  overall_figures$day_minus_1_HOSPITAL_score,
  overall_figures$day_minus_2_HOSPITAL_score,
  overall_figures$admission_HOSPITAL_score,
  overall_figures$discharge_news_score,
  overall_figures$day_minus_1_news_score,
  overall_figures$day_minus_2_news_score,
  overall_figures$admission_news_score,
  overall_figures$age_at_encounter,
  overall_figures$sex_recode['Male'],
  overall_figures$sex_recode['Female'],
  overall_figures$sex_recode['Missing'],
  overall_figures$race_category['White or Caucasian'],
  overall_figures$race_category['Black or African American'],
  overall_figures$race_category['Asian'],
  overall_figures$race_category['American Indian or Alaska Native'],
  overall_figures$race_category['Native Hawaiian and Other Pacific Islander'],
  overall_figures$race_category['More than one race'],
  overall_figures$race_category['Other'],
  overall_figures$race_category['Missing'],
  overall_figures$ethnicity_recode['Hispanic'],
  overall_figures$ethnicity_recode['Non-Hispanic'],
  overall_figures$ethnicity_recode['Missing'],
  overall_figures$language_category['English'],
  overall_figures$language_category['Spanish'],
  overall_figures$language_category['Other'],
  overall_figures$language_category['Missing'],
  overall_figures$ICU_category['Yes'],
  overall_figures$ICU_category['No'],
  overall_figures$dc_disp_category['Home'],
  overall_figures$dc_disp_category['Post-acute care'],
  overall_figures$dc_disp_category['AMA'],
  overall_figures$dc_disp_category['Hospice'],
  overall_figures$dc_disp_category['Other'],
  overall_figures$dc_disp_category['Missing'],
  overall_figures$discharge_service['Medicine'],
  overall_figures$discharge_service['Orthopedics'],
  overall_figures$discharge_service['Surgery'],
  overall_figures$discharge_service['Other'],
  overall_figures$discharge_service['Missing'],
  overall_figures$patient_class['Inpatient'],
  overall_figures$patient_class['Observation'],
  overall_figures$facility_name['BH'],
  overall_figures$facility_name['GH'],
  overall_figures$facility_name['GVH'],
  overall_figures$facility_name['HRH'],
  overall_figures$facility_name['LPH'],
  overall_figures$facility_name['MCR'],
  overall_figures$facility_name['MHC'],
  overall_figures$facility_name['MHN'],
  overall_figures$facility_name['PPRH'],
  overall_figures$facility_name['PVH'],
  overall_figures$facility_name['UCH'],
  overall_figures$facility_name['YVMC'],
  overall_figures$facility_name['CHCO AT MHN HOSPITAL'],
  overall_figures$los_in_hours,
  overall_figures$CMR_Index_Readmission_Zero,
  overall_figures$CMR_Index_Readmission_NonZero,
  overall_figures$CMR_Index_Mortality_Zero,
  overall_figures$CMR_Index_Mortality_NonZero
)

final_table <- combined_table[,c(1,2,9,5,3,4,8,7,6)] %>% 
  mutate(variable = case_when(
    variable == "readmission_30days_recode" ~ "30-Day Readmission",
    variable == "ed_30days_recode" ~ "30-Day ED Readmission",
    variable == "death_30_days" ~ "30-Day Mortality",
    variable == "sex_recode" ~ "Sex",
    variable == "race_category" ~ "Race",
    variable == "ethnicity_recode" ~ "Ethnicity",
    variable == "language_category" ~ "Primary Language",
    variable == "ICU_category" ~ "Visited ICU",
    variable == "dc_disp_category" ~ "Discharge Disposition",
    variable == "discharge_service" ~ "Discharge Service",
    variable == "patient_class" ~ "Patient Class",
    variable == "facility_name" ~ "Facility",
    variable == "CMR_Index_Readmission_Zero" ~ "CMR Mortality Index (Zero)",
    variable == "CMR_Index_Readmission_NonZero" ~ "CMR Mortality Index (Non-Zero)",
    variable == "CMR_Index_Mortality_Zero" ~ "CMR Mortality Index (Zero)",
    variable == "CMR_Index_Mortality_NonZero" ~ "CMR Mortality Index (Non-Zero)",
    variable == "admission_HOSPITAL_score" ~ "HOSPITAL Score at Admission",
    variable == "admission_news_score" ~ "NEWS2 Score at Admission",
    variable == "age_at_encounter" ~ "Age",
    variable == "day_minus_1_HOSPITAL_score" ~ "HOSPITAL Score 1-Day Before Discharge",
    variable == "day_minus_1_news_score" ~ "NEWS2 Score 1-Day Before Discharge",
    variable == "day_minus_2_HOSPITAL_score" ~ "HOSPITAL Score 2-Days Before Discharge",
    variable == "day_minus_2_news_score" ~ "NEWS2 Score 2-Days Before Discharge",
    variable == "discharge_HOSPITAL_score" ~ "HOSPITAL Score at Discharge",
    variable == "discharge_news_score" ~ "NEWS2 Score at Discharge",
    variable == "los_in_hours" ~ "Length of Stay (hours)"
  ))

names(final_table) <- c("Variable", "Value", "Overall (n=72,983)", 
                       "Medicare (n=36,100)", "Commercial (n=17,266)",
                       "Medicaid (n=13,214)", "Self-Pay (n=2,260)",
                       "Other (n=4,069)", "Missing (n=74)")

final_table_short <- final_table[c(1,3,5,7:14),-2] %>% 
  gt() %>% 
  tab_header(
    title = "Table 1: Descriptive Statistics of Outcome and Score Variables by Payor Category"
  ) %>% 
  tab_options(table.font.size = px(11))
  
write.csv(final_table_short, "final_table_short.csv", row.names = FALSE)
