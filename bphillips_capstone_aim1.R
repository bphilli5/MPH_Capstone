###############################################################################
#> bphillips_capstone_aim1
#> Analysis related to the primary aim: determining the effect of insurance
#> status on the relationship between death/readmission risk and physiological 
#> scores.
###############################################################################

#> Step 1: Recoding and variable selection
#> Step 2: Defining independent variables
#> Step 3: Bivariate analysis
#> Step 4: Collinearity of numeric predictors
#> Step 5: Plotting of numeric predictors and outcomes
#> Step 6: Creating simple additive models
#> Step 7: Creating simple interaction models
#> Step 8: Type III testing

###############################################################################
# Step 1: Recoding and variable selection
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
         across(c(payor_category,
                   sex_recode,
                   race_category,
                   ethnicity_recode,
                   language_category,
                   ICU_category,
                   dc_disp_category,
                   discharge_service,
                   patient_class,
                   facility_name),
                 ~factor(.),
                 .names="{.col}_analysis")) %>% 
  
  select(
    # Outcomes
    readmission_30days_recode_analysis,
    ed_30days_recode_analysis,
    death_30_days_analysis,
    # Scores
    discharge_HOSPITAL_score,
    day_minus_1_HOSPITAL_score,
    day_minus_2_HOSPITAL_score,
    admission_HOSPITAL_score,
    average_HOSPITAL_score,
    discharge_news_score,
    day_minus_1_news_score,
    day_minus_2_news_score,
    admission_news_score,
    average_NEWS_score,
    # Categorical predictors
    payor_category_analysis,
    sex_recode_analysis,
    race_category_analysis,
    ethnicity_recode_analysis,
    language_category_analysis,
    ICU_category_analysis,
    dc_disp_category_analysis,
    discharge_service_analysis,
    patient_class_analysis,
    facility_name_analysis,
    # Numeric predictors
    age_at_encounter,
    los_in_hours,
    CMR_Index_Readmission,
    CMR_Index_Mortality
         ) %>% 

  drop_na(  
    payor_category_analysis,
    sex_recode_analysis,
    race_category_analysis,
    ethnicity_recode_analysis,
    language_category_analysis,
    ICU_category_analysis,
    dc_disp_category_analysis,
    discharge_service_analysis,
    patient_class_analysis,
    facility_name_analysis,
    # Numeric predictors
    age_at_encounter,
    los_in_hours,
    CMR_Index_Readmission,
    CMR_Index_Mortality
  ) %>% 
  
  mutate(across(c(discharge_HOSPITAL_score,
                  day_minus_1_HOSPITAL_score,
                  day_minus_2_HOSPITAL_score,
                  admission_HOSPITAL_score,
                  discharge_news_score,
                  day_minus_1_news_score,
                  day_minus_2_news_score,
                  admission_news_score),
                        ~factor(.),
                        .names="{.col}_factor")
  )


d###############################################################################
# Step 2: Defining independent variables for HOSPITAL and NEWS models 
###############################################################################
all_variables <- names(d_analysis)
outcome_variables <- names(d_analysis)[1:3]
score_variables <- names(d_analysis)[4:13]
predictors <- names(d_analysis)[14:length(d_analysis)]
hospital_variables <- data.frame(
  variable = c(score_variables[1:6],
               predictors)
)
news_variables <- data.frame(
  variable = c(score_variables[6:10],
               predictors)
)

###############################################################################
# Step 3: Bivariate analysis
###############################################################################
# Function to produce bivariate regression results  for outcomes and predictors
bivariate_glm <- function(data, outcome, predictors) {
  results <- list()
  for (predictor in predictors) {
    data <- data %>% 
      filter(!is.na(data[[predictor]]))
    formul <- as.formula(paste(outcome, "~ 1"))
    uni_model <- glm(formul, data = data, family = binomial)
    formula <- as.formula(paste(outcome, "~", predictor))
    model <- glm(formula, data = data, family = binomial)
    results$model[[predictor]] <- tidy(model)[-1,c(1,2,5)]
    results$lrt[[predictor]] <- tidy(anova(uni_model, model,test="LRT"))[-1,c(1,5,6)]
  }
  return(results)
}

# Perform bivariate GLM analysis for each outcome
readmission_results <- bivariate_glm(d_analysis, 
                                     outcome_variables[1], 
                                     c(score_variables,predictors))
ed_results <- bivariate_glm(d_analysis,
                            outcome_variables[2], 
                            c(score_variables,predictors))
death_results <- bivariate_glm(d_analysis, 
                               outcome_variables[3], 
                               c(score_variables,predictors))

# Create results tables and view
view(readmission_table_models <- do.call(rbind, readmission_results$model) %>% 
       mutate(across(where(is.numeric), round, 3)))
view(ed_table_models <- do.call(rbind, ed_results$model) %>% 
       mutate(across(where(is.numeric), round, 3)))
view(death_table_models <- do.call(rbind, death_results$model) %>% 
       mutate(across(where(is.numeric), round, 3)))

# Create LRT tables and view
view(readmission_table_lrt <- do.call(rbind, readmission_results$lrt) %>% 
       mutate(across(where(is.numeric), round, 3)))
view(ed_table_lrt <- do.call(rbind, ed_results$lrt) %>% 
       mutate(across(where(is.numeric), round, 3)))
view(death_table_lrt <- do.call(rbind, death_results$lrt) %>% 
       mutate(across(where(is.numeric), round, 3)))

###############################################################################
# Step 4: Plotting of numeric predictors and outcomes
###############################################################################
# Function to iterate over predictors and plot numerics against outcomes
plot_linear_association <- function(data, outcome, predictors) {
  results <- list()
  logit <- function(pr) log(pr/(1-pr))
  for (predictor in predictors) {
    if (is.numeric(data[[predictor]])) {
      loessfit <- predict(loess(data[[outcome]]~data[[predictor]]))
      pi <- pmax(pmin(loessfit,0.9999),0.0001)
      logitfitted <- logit(pi)
      o <- order(data[[predictor]])
      plot_data <- data.frame(x = data[[predictor]][o],
                                                   y = logitfitted[o])
      plot <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_line() +
        labs(x = predictor)
      results[[predictor]] <- plot
    }
  }
  return(results)
}

readmission_plots <- plot_linear_association(d_analysis,
                                             outcome_variables[1],
                                             predictors)
readmission_plot_grid <- grid.arrange(grobs=readmission_plots, 
                                      top="30-Day Readmission")

ed_plots <- plot_linear_association(d_analysis,
                                    outcome_variables[2],
                                    predictors)
ed_plot_grid <- grid.arrange(grobs=ed_plots, 
                             top="30-Day ED Readmission")

death_plots <- plot_linear_association(d_analysis,
                                       outcome_variables[3],
                                       predictors)
death_plot_grid <- grid.arrange(grobs=death_plots, 
                                top="30-Day Death")

# Testing score variables
readmission_plots <- plot_linear_association(d_analysis,
                                             outcome_variables[1],
                                             c(score_variables, predictors))
readmission_plot_grid <- grid.arrange(grobs=readmission_plots, 
                                      top="30-Day Readmission")

# Log transformations
# plot_log_association <- function(data, outcome, predictors) {
#   results <- list()
#   logit <- function(pr) log(pr/(1-pr))
#   for (predictor in predictors) {
#     if (is.numeric(data[[predictor]]) && all(data[[predictor]] >= 0)) {
#       data[[predictor]] <- log(data[[predictor]])
#       loessfit <- predict(loess(data[[outcome]]~data[[predictor]]))
#       pi <- pmax(pmin(loessfit,0.9999),0.0001)
#       logitfitted <- logit(pi)
#       o <- order(data[[predictor]])
#       plot_data <- data.frame(x = data[[predictor]][o],
#                               y = logitfitted[o])
#       plot <- ggplot(plot_data, aes(x = x, y = y)) +
#         geom_line() +
#         labs(x = predictor)
#       results[[predictor]] <- plot
#     }
#   }
#   return(results)
# }
# readmission_log_plots <- plot_log_association(d_analysis,
#                                              outcome_variables[1],
#                                              predictors)
# readmission_log_plot_grid <- grid.arrange(grobs=readmission_log_plots, 
#                                       top="30-Day Readmission - Log")
# 
# ed_log_plots <- plot_log_association(d_analysis,
#                                      outcome_variables[2],
#                                      predictors)
# ed_log_plot_grid <- grid.arrange(grobs=ed_log_plots, 
#                                  top="30-Day ED Readmission - Log")
# 
# death_log_plots <- plot_log_association(d_analysis,
#                                         outcome_variables[3],
#                                         predictors)
# death_log_plot_grid <- grid.arrange(grobs=death_log_plots, 
#                                     top="30-Day Death - Log")

###############################################################################
# Step 4: Assesing assumption of linearity for indices
###############################################################################

index_variables <- d_analysis[]


###############################################################################
# Step 5: Collinearity of numeric predictors
###############################################################################

# NEWS model collinearity
# Subset data
news_subset <- d_analysis[, news_variables$variable]
news_numeric_vars <- sapply(news_subset, is.numeric)
news_numeric_subset <- names(news_subset[, news_numeric_vars])

# Calculate the correlation matrix for news_numeric_subset
news_correlation_matrix <- round(cor(d_analysis[news_numeric_subset], 
                                     use='pairwise.complete.obs'),3)

# View NEWS correlation matrix
view(news_correlation_matrix)

# HOSPITAL model collinearity
# Subset data
hospital_subset <- d_analysis[, hospital_variables$variable]
hospital_numeric_vars <- sapply(hospital_subset, is.numeric)
hospital_numeric_subset <- hospital_subset[, hospital_numeric_vars]

# Calculate the correlation matrix for hospital_numeric_subset
hospital_correlation_matrix <- round(cor(hospital_numeric_subset, 
                                         use='pairwise.complete.obs'),3)

# View HOSPITAL correlation matrix
view(hospital_correlation_matrix)

###############################################################################
# Step 5: Creating simple additive models
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

###############################################################################
# Step 7: Type III Testing
###############################################################################

# Additive model type III testing
# Apply test for additive fits
drop1_list_1 <- lapply(fits_1, function(fit) {
  drop1(fit, test = "LRT")
})

# Creating a table of p-values from the Type III tests
drop1_table_1 <- do.call(cbind, lapply(drop1_list_1, function(x) x$`Pr(>Chi)`[-1]))

# Including significance in tables
drop1_table_starred_1 <- apply(drop1_table_1, 2, function(x) {
  case_when(
    x >= 0.1 ~ as.character(round(x, 3)),
    x >= 0.05 ~ paste0(round(x, 3), " -"),
    x >= 0.01 ~ paste0(round(x, 3), " **"),
    x >= 0.001 ~ paste0(round(x, 3), " ***"),
    TRUE ~ "<0.001 ****"
  )
})

# Naming tables
table_row_names <- c("Discharge Score",
                     "Payor Category",
                     "Sex",
                     "Race",
                     "Ethnicity",
                     "Primary Language",
                     "Visited ICU",
                     "Discharge Disposition",
                     "Discharge Service",
                     "Patient Class",
                     "Facility Name",
                     "Age",
                     "Log Length of Stay",
                     "CMR Readmission Index",
                     "CMR Mortality Index",
                     "Discharge*Payor")

table_column_names <- c("HOSPITAL Readmission", 
                        "HOSPITAL ED Visit",
                        "HOSPITAL Death",
                        "NEWS Readmission",
                        "NEWS ED Visit",
                        "NEWS Death")

rownames(drop1_table_starred_1) <- table_row_names[-16]
colnames(drop1_table_starred_1) <- table_column_names
view(drop1_table_starred_1)

# Interatctive model Type III testing
# Perform drop1() on each model fit
drop1_list_i <- lapply(fits_i, function(fit) {
  drop1(fit, test = "LRT")
})

# Creating a table of p-values from the Type III tests
drop1_table_i <- do.call(cbind, lapply(drop1_list_i, function(x) x$`Pr(>Chi)`[-1]))

# Including significance in tables
drop1_table_starred_i <- apply(drop1_table_i, 2, function(x) {
  case_when(
    x >= 0.1 ~ as.character(round(x, 3)),
    x >= 0.05 ~ paste0(round(x, 3), " -"),
    x >= 0.01 ~ paste0(round(x, 3), " **"),
    x >= 0.001 ~ paste0(round(x, 3), " ***"),
    TRUE ~ "<0.001 ****"
  )
})

rownames(drop1_table_starred_i) <- table_row_names[3:length(table_row_names)]
colnames(drop1_table_starred_i) <- table_column_names
view(drop1_table_starred_i)

###
# VIFs
###
fit_1_vifs <- lapply(fits_1, function(x) vif(x))

fit_i_vifs <- lapply(fits_i, function(x) vif(x))
