###############################################################################
#> bphillips_capstone_aim1
#> Analysis related to the primary aim: determining the effect of insurance
#> status on the relationship between death/readmission risk and physiological 
#> scores.
###############################################################################

#> Step 1: Recoding and variable selection
#> Step 2: Defining independent variables
#> Step 3: Bivariate analysis TODO
#> Step 4: Creating additive models
#> Step 5: Purposeful selection of variables
#> Step 6: Creating interactive models TODO

###############################################################################
# Step 1: Recoding to make R formatting happy and variable selection
###############################################################################
d_analysis <- d_tables %>% 
  mutate(across(c(readmission_30days_recode,
                  ed_30days_recode,
                  death_30_days),
                ~case_when(
                  .== "No" ~ 0,
                  .== "Yes" ~ 1,
                  .default=NULL),
                .names="{.col}_analysis"),
         (across(c(payor_category,
                   sex_recode,
                   race_category,
                   ethnicity_recode,
                   language_category,
                   ICU_days_recode,
                   dc_disp_category,
                   discharge_service,
                   facility_name),
                 ~factor(.),
                 .names="{.col}_analysis"))) %>% 
  
  select(readmission_30days_recode_analysis,
         ed_30days_recode_analysis,
         death_30_days_analysis,
         
         payor_category_analysis,
         sex_recode_analysis,
         race_category_analysis,
         ethnicity_recode_analysis,
         language_category_analysis,
         ICU_days_recode_analysis,
         dc_disp_category_analysis,
         discharge_service_analysis,
         facility_name_analysis,
         
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
# Step 2: Defining predictor variables for HOSPITAL and NEWS models 
###############################################################################
predictors <- c("discharge_HOSPITAL_score",
                "discharge_news_score",
                "payor_category_analysis", 
                "sex_recode_analysis",
                "race_category_analysis", 
                "ethnicity_recode_analysis", 
                "language_category_analysis",
                "ICU_days_recode_analysis", 
                "dc_disp_category_analysis",
                "discharge_service_analysis",
                "facility_name_analysis",
                "age_at_encounter",
                "admission_HOSPITAL_score", 
                "day_minus_2_HOSPITAL_score", 
                "day_minus_1_HOSPITAL_score",
                "admission_news_score", 
                "day_minus_2_news_score", 
                "day_minus_1_news_score")

hospital_variables <- data.frame(
  variable = c("discharge_HOSPITAL_score", 
               "payor_category_analysis", 
               "sex_recode_analysis",
               "race_category_analysis", 
               "ethnicity_recode_analysis", 
               "language_category_analysis",
               "ICU_days_recode_analysis", 
               "dc_disp_category_analysis", 
               "discharge_service_analysis",
               "facility_name_analysis",
               "age_at_encounter",
               "admission_HOSPITAL_score", 
               "day_minus_2_HOSPITAL_score", 
               "day_minus_1_HOSPITAL_score")
)

news_variables <- data.frame(
  variable = c("discharge_HOSPITAL_score",
               "payor_category_analysis", 
               "sex_recode_analysis",
               "race_category_analysis", 
               "ethnicity_recode_analysis", 
               "language_category_analysis",
               "ICU_days_recode_analysis", 
               "dc_disp_category_analysis", 
               "discharge_service_analysis",
               "facility_name_analysis",
               "age_at_encounter",
               "admission_news_score", 
               "day_minus_2_news_score", 
               "day_minus_1_news_score")
)

###############################################################################
# Step 3: Bivariate analysis
###############################################################################
bivariate_glm <- function(data, outcome, predictors) {
  results <- list()
  for (predictor in predictors) {
    formula <- as.formula(paste(outcome, "~", predictor))
    model <- glm(formula, data = data, family = binomial)
    results[[predictor]] <- summary(model)
  }
  return(results)
}

# Perform bivariate GLM analysis for each outcome
readmission_results <- bivariate_glm(d_analysis, "readmission_30days_recode_analysis", predictors)
ed_results <- bivariate_glm(d_analysis, "ed_30days_recode_analysis", predictors)
death_results <- bivariate_glm(d_analysis, "death_30_days_analysis", predictors)

###############################################################################
# Step 4: Creating 6 models for the 3 endpoints and 2 scoring systems
###############################################################################
model_formulas <- list(
  hospital_read <- paste0("readmission_30days_recode_analysis ~ ",
                          paste(hospital_variables$variable, collapse = " + ")),
  hospital_ed_visit <- paste0("ed_30days_recode_analysis ~ ",
                              paste(hospital_variables$variable, collapse = " + ")),
  hospital_death <- paste0("death_30_days_analysis ~ ",
                           paste(hospital_variables$variable, collapse = " + ")),
  news_readmission = paste0("readmission_30days_recode_analysis ~ ",
                            paste(news_variables$variable, collapse = " + ")),
  news_ed_visit = paste0("ed_30days_recode_analysis ~ ",
                         paste(news_variables$variable, collapse = " + ")),
  news_death = paste0("death_30_days_analysis ~ ",
                      paste(news_variables$variable, collapse = " + "))
)

fits_1 <- lapply(model_formulas, function(formula) {
  glm(formula, data = d_analysis, family = binomial)
})

###############################################################################
# Step 5: Purposeful variable selection
###############################################################################

# Step 5a: Type III testing
drop1_list <- lapply(fits_1, function(fit) {
  drop1(fit, test = "LRT")
})

# Creating a table of p-values from the Type III tests
drop1_table <- do.call(cbind, lapply(drop1_list, function(x) x$`Pr(>Chi)`[-1]))

# Including significance signifiers
drop1_table_starred <- apply(drop1_table, 2, function(x) {
  case_when(
    x >= 0.1 ~ as.character(round(x, 3)),
    x >= 0.05 ~ paste0(round(x, 3), " -"),
    x >= 0.01 ~ paste0(round(x, 3), " **"),
    x >= 0.001 ~ paste0(round(x, 3), " ***"),
    TRUE ~ "<0.001 ****"
  )
})

# Naming tables
rownames(drop1_table_starred) <- c("Discharge Scores",
                                   "Payor Category",
                                   "Sex",
                                   "Race",
                                   "Ethnicity",
                                   "Primary Language",
                                   "Visited ICU",
                                   "Discharge Disposition",
                                   "Discharge Service",
                                   "Facility Name",
                                   "Age",
                                   "Admission Scores",
                                   "Day -1 Scores",
                                   "Day -2 Scores")
        
colnames(drop1_table_starred) <- c("HOSPITAL Readmission", 
                                   "HOSPITAL ED Visit",
                                   "HOSPITAL Death",
                                   "NEWS Readmission",
                                   "NEWS ED Visit",
                                   "NEWS Death")

# View table
view(drop1_table_starred)

# Step 5b: Correlation matrices for HOSPITAL and NEWS scores
news_subset <- d_analysis[, news_variables$variable]

# Subset news_subset to include only numeric variables
news_numeric_vars <- sapply(news_subset, is.numeric)
news_numeric_subset <- news_subset[, news_numeric_vars]

# Calculate the correlation matrix for news_numeric_subset
news_correlation_matrix <- round(cor(news_numeric_subset, 
                                     use='pairwise.complete.obs'),3)

# View NEWS correlation matrix
view(news_correlation_matrix)

hospital_subset <- d_analysis[, hospital_variables$variable]

# Subset hospital_subset to include only numeric variables
hospital_numeric_vars <- sapply(hospital_subset, is.numeric)
hospital_numeric_subset <- hospital_subset[, hospital_numeric_vars]

# Calculate the correlation matrix for hospital_numeric_subset
hospital_correlation_matrix <- round(cor(hospital_numeric_subset, 
                                     use='pairwise.complete.obs'),3)

d_analysis_filtered <- subset(d_analysis, dc_disp_category_analysis != "Missing")

# View HOSPITAL correlation matrix
view(hospital_correlation_matrix)

attributes(alias(fits_1[[1]])$Complete)$dimnames[[1]]

vif(fits_1[[4]])

###############################################################################
# Step 6: Fitting the interactive models
###############################################################################

model_formulas_i <- list(
  hospital_read <- paste0("readmission_30days_recode_analysis ~ discharge_HOSPITAL_score * ",
                          paste(hospital_variables$variable[2:length(hospital_variables$variable)], collapse = " + ")),
  hospital_ed_visit <- paste0("ed_30days_recode_analysis ~ discharge_HOSPITAL_score * ",
                              paste(hospital_variables$variable[2:length(hospital_variables$variable)], collapse = " + ")),
  hospital_death <- paste0("death_30_days_analysis ~ discharge_HOSPITAL_score * ",
                           paste(hospital_variables$variable[2:length(hospital_variables$variable)], collapse = " + ")),
  news_readmission = paste0("readmission_30days_recode_analysis ~ discharge_HOSPITAL_score * ",
                            paste(news_variables$variable[2:length(hospital_variables$variable)], collapse = " + ")),
  news_ed_visit = paste0("ed_30days_recode_analysis ~ discharge_HOSPITAL_score * ",
                         paste(news_variables$variable[2:length(hospital_variables$variable)], collapse = " + ")),
  news_death = paste0("death_30_days_analysis ~ discharge_HOSPITAL_score * ",
                      paste(news_variables$variable[2:length(hospital_variables$variable)], collapse = " + "))
)

fits_i <- lapply(model_formulas_i, function(formula) {
  glm(formula, data = d_analysis, family = binomial)
})

# Perform drop1() on each model fit
drop1_list_i <- lapply(fits_i, function(fit) {
  drop1(fit, test = "LRT")
})

# Create the drop1_table using do.call() and cbind()
drop1_table_i <- do.call(cbind, lapply(drop1_list_i, function(x) x$`Pr(>Chi)`[-1]))

# Define the row and column names separately and assign them to the drop1_table
### TO DO: match interaction coeffificnets to rows. Move ordering to last
custom_order_i <- c("Discharge Scores",
                  "Payor Category",
                  "Discharge*Payor",
                  "Day -1 Scores", 
                  "Day -2 Scores",
                  "Admission Scores",
                  "Age",
                  "Sex",
                  "Race",
                  "Ethnicity",
                  "Primary Language",
                  "Visited ICU",
                  "Discharge Disposition",
                  "Discharge Service",
                  "Facility Name")

# Order the rows of drop1_table using dplyr functions
drop1_table_ordered_i <- drop1_table_i %>%
  as.data.frame() %>%
  slice(match(custom_order, rownames(drop1_table_i))) %>%
  as.matrix()

# Simplify the ifelse() statements in the drop1_table_starred creation
drop1_table_starred_i <- apply(drop1_table_ordered_i, 2, function(x) {
  case_when(
    x >= 0.1 ~ as.character(round(x, 3)),
    x >= 0.05 ~ paste0(round(x, 3), " -"),
    x >= 0.01 ~ paste0(round(x, 3), " **"),
    x >= 0.001 ~ paste0(round(x, 3), " ***"),
    TRUE ~ "<0.001 ****"
  )
})

rownames(drop1_table_starred_i) <- c("Discharge Scores",
                                   "Payor Category",
                                   "Sex",
                                   "Race",
                                   "Ethnicity",
                                   "Primary Language",
                                   "Visited ICU",
                                   "Discharge Disposition",
                                   "Discharge Service",
                                   "Facility Name",
                                   "Age",
                                   "Admission Scores",
                                   "Day -1 Scores",
                                   "Day -2 Scores")

colnames(drop1_table_starred) <- c("HOSPITAL Readmission", 
                                   "HOSPITAL ED Visit",
                                   "HOSPITAL Death",
                                   "NEWS Readmission",
                                   "NEWS ED Visit",
                                   "NEWS Death")

view(drop1_table_starred)
