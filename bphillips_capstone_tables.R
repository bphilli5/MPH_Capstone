###############################################################################
#> bphillips_capstone_tables
#> Creating descriptive statistics tables
###############################################################################

#> Step 1: Variable recoding and selection
#> Step 2: Sorting tables
#> Step 3: Creating categorical variable table
#> Step 4: Creating numeric variable table
#> Step 5: ?????

###############################################################################
# Step 1: Categorical variable recoding + variable selection 
###############################################################################

d_tables <- d_sas %>% 
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
         ICU_days_recode = ifelse(ICU_days == "","No", "Yes")) %>% 
  
  select(readmission_30days_recode,
         ed_30days_recode,
         death_30_days,
         payor_category,
         sex_recode,
         race_category,
         ethnicity_recode,
         language_category,
         ICU_days_recode,
         dc_disp_category,
         payor_category,
         age_at_encounter,
         admission_HOSPITAL_score,
         admission_news_score,
         day_minus_2_HOSPITAL_score,
         day_minus_1_HOSPITAL_score,
         day_minus_1_news_score,
         day_minus_2_news_score,
         discharge_HOSPITAL_score,
         discharge_news_score)

###############################################################################
# Step 2: Setting up the sorting of the tables
###############################################################################

table_variable_order <- c("readmission_30days_recode",
                          "ed_30days_recode",
                          "death_30_days",
                          
                          "age_at_encounter",
                          "sex_recode",
                          "race_category",
                          "ethnicity_recode",
                          "language_category",
                          "ICU_days_recode",
                          "dc_disp_category",
                          
                          "discharge_HOSPITAL_score",
                          "day_minus_1_HOSPITAL_score",
                          "day_minus_2_HOSPITAL_score",
                          "admission_HOSPITAL_score",
                          "discharge_news_score",
                          "day_minus_1_news_score",
                          "day_minus_2_news_score",
                          "admission_news_score")

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
         ICU_days_recode,
         dc_disp_category) %>%
  
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
  arrange(match(variable, table_variable_order), 
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
         discharge_news_score) %>% 
  pivot_longer(-payor_category, names_to = "variable", values_to = "value") %>% 
  group_by(payor_category, variable) %>% 
  summarise(
    mean = round(mean(value, na.rm = TRUE), 1),
    sd = round(sd(value, na.rm = TRUE), 1)
  ) %>% 
  mutate(result = paste0(mean, " (", sd, ")")) %>% 
  select(-mean, -sd) %>% 
  pivot_wider(names_from = payor_category, 
              values_from = result, values_fill = "0 (0)")

# View tables
view(categorical_table)
view(numeric_table)

