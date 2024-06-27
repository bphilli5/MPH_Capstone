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
    # los_in_hours,
    CMR_Index_Readmission,
    CMR_Index_Mortality,
    age_at_encounter
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
    # los_in_hours,
    CMR_Index_Readmission,
    CMR_Index_Mortality
  ) %>% 
  
  # Creating factored scores
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
  ) %>%
  
  # Creating scores cut by 3 quantiles
  mutate(across(c(discharge_HOSPITAL_score,
                  day_minus_1_HOSPITAL_score,
                  day_minus_2_HOSPITAL_score,
                  admission_HOSPITAL_score,
                  discharge_news_score,
                  day_minus_1_news_score,
                  day_minus_2_news_score,
                  admission_news_score), 
                .fns = ~cut(., 
                            breaks = quantile(., probs = seq(0, 1, 1/3), na.rm = TRUE),
                            labels = paste0("Q", 1:3),
                            include.lowest = TRUE),
                .names = "{.col}_quantile")
  ) %>% 
  
  # Creating average of d, d-1, d-2 scores
  mutate(discharge_HOSPITAL_score_avg = 
           if_else(!is.na(discharge_HOSPITAL_score) & 
                     !is.na(day_minus_1_HOSPITAL_score) & 
                     !is.na(day_minus_2_HOSPITAL_score),
                   rowMeans(cbind(discharge_HOSPITAL_score,
                                  day_minus_1_HOSPITAL_score,
                                  day_minus_2_HOSPITAL_score)),
                                  NA),
         discharge_news_score_avg = 
           if_else(!is.na(discharge_news_score) & 
                     !is.na(day_minus_1_news_score) & 
                     !is.na(day_minus_2_news_score),
                   rowMeans(cbind(discharge_news_score,
                                  day_minus_1_news_score,
                                  day_minus_2_news_score)),
                                  NA)
  ) %>% 
  rowwise() %>%
  mutate(hospital_trend = {
    x <- c(1, 2, 3)  # Representing the three days
    y <- c(day_minus_2_HOSPITAL_score, 
           day_minus_1_HOSPITAL_score, 
           discharge_HOSPITAL_score)
    if (all(!is.na(y))) {
      coef(lm(y ~ x))[2]  # Slope of the linear regression
    } else {
      NA
    }
  }) %>%
  mutate(news_trend = {
    x <- c(1, 2, 3)  # Representing the three days
    y <- c(day_minus_2_news_score, 
           day_minus_1_news_score, 
           discharge_news_score)
    if (all(!is.na(y))) {
      coef(lm(y ~ x))[2]  # Slope of the linear regression
    } else {
      NA
    }
  }) %>%
  ungroup()

###############################################################################
# Step 2: Defining independent variables for HOSPITAL and NEWS models 
###############################################################################
all_variables <- names(d_analysis)
outcome_variables <- names(d_analysis)[1:3]
score_variables <- names(d_analysis)[4:13]
score_variable_factor <- names(d_analysis)[27:34]
score_variable_quantile <- names(d_analysis)[35:42]
score_variable_averages <- names(d_analysis)[43:44]
score_variable_trend <- names(d_analysis)[45:46]
predictors <- names(d_analysis)[14:26]

hospital_scores <- c(score_variables[1:5],
                     score_variable_factor[1:4],
                     score_variable_quantile[1:4],
                     score_variable_averages[1],
                     score_variable_trend[1])

news_scores <- c(score_variables[6:10],
                 score_variable_factor[5:8],
                 score_variable_quantile[5:8],
                 score_variable_averages[2],
                 score_variable_trend[2])

hospital_variables <- data.frame(
  variable = c(hospital_scores,
               predictors)
)
news_variables <- data.frame(
  variable = c(news_scores,
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

# Perform bivariate GLM analysis for each outcomeee
readmission_results <- bivariate_glm(d_analysis, 
                                     outcome_variables[1], 
                                     c(predictors,
                                       hospital_scores,
                                       news_scores))
ed_results <- bivariate_glm(d_analysis,
                            outcome_variables[2], 
                            c(predictors,
                              hospital_scores,
                              news_scores))
death_results <- bivariate_glm(d_analysis, 
                               outcome_variables[3], 
                               c(predictors,
                                 hospital_scores,
                                 news_scores))

# Create results tables and view
readmission_table_models <- do.call(rbind, readmission_results$model)
ed_table_models <- do.call(rbind, ed_results$model)
death_table_models <- do.call(rbind, death_results$model)

# Create LRT tables
readmission_table_lrt <- do.call(rbind, readmission_results$lrt)
ed_table_lrt <- do.call(rbind, ed_results$lrt)
death_table_lrt <- do.call(rbind, death_results$lrt)

# Based on the results of the bivariate analysis, ICU_category can be removed
# from models of readmission risk
read_predictors <- read_predictors[-6]


# Based on the results of bivariate analysis, NEWS2 doe not appear to be a 
# significant predictor of ED readmission


# Removing ethnicity as a predictor
death_predictors <- death_predictors[-4]

###############################################################################
# Step 5: Comparing methods of modeling scores
###############################################################################

readmission_score_comps <- cbind(readmission_table_lrt[c(14:17,28:31),], # Numeric
                                 readmission_table_lrt[c(19:22,33:36),], # Factor
                                 readmission_table_lrt[c(23:26,37:40),], # Quantiles
                                 readmission_table_lrt[c(18,27,32,41),] # Averages
)

col_names_comps <- c("Nums", "Nums Dev", "Nums p",
                     "Fac", "Fac Dev", "Fac p",
                     "Quants", "Quants Dev", "Quants p",
                     "Avgs", "Avgs Dev", "Avgs p")

colnames(readmission_score_comps) <- col_names_comps

readmission_score_comps <- readmission_score_comps[-c(4,7,10)] %>% 
  mutate(Nums = sub(".*~", "", Nums),
         across(c(2,4,6,8), ~round(.,1)),
         across(c(3,5,7,9), ~round(.,4))
         )

ed_score_comps <- cbind(ed_table_lrt[c(14:17,28:31),], # Numeric
                                 ed_table_lrt[c(19:22,33:36),], # Factor
                                 ed_table_lrt[c(23:26,37:40),], # Quantiles
                                 ed_table_lrt[c(18,27,32,41),] # Averages
)

col_names_comps <- c("Nums", "Nums Dev", "Nums p",
                     "Fac", "Fac Dev", "Fac p",
                     "Quants", "Quants Dev", "Quants p",
                     "Avgs", "Avgs Dev", "Avgs p")

colnames(ed_score_comps) <- col_names_comps

ed_score_comps <- ed_score_comps[-c(4,7,10)] %>% 
  mutate(Nums = sub(".*~", "", Nums),
         across(c(2,4,6,8), ~round(.,1)),
         across(c(3,5,7,9), ~round(.,4))
  )

death_score_comps <- cbind(death_table_lrt[c(14:17,28:31),], # Numeric
                                 death_table_lrt[c(19:22,33:36),], # Factor
                                 death_table_lrt[c(23:26,37:40),], # Quantiles
                                 death_table_lrt[c(18,27,32,41),] # Averages
)

col_names_comps <- c("Nums", "Nums Dev", "Nums p",
                     "Fac", "Fac Dev", "Fac p",
                     "Quants", "Quants Dev", "Quants p",
                     "Avgs", "Avgs Dev", "Avgs p")

colnames(death_score_comps) <- col_names_comps

death_score_comps <- death_score_comps[-c(4,7,10)] %>% 
  mutate(Nums = sub(".*~", "", Nums),
         across(c(2,4,6,8), ~round(.,1)),
         across(c(3,5,7,9), ~round(.,4))
  )


###############################################################################
# Step 5: Bivariate analysis with payor category
###############################################################################

bivariate_analysis <- function(data, outcome, predictors) {
  results <- list()
  
  for (predictor in predictors) {
    data_subset <- data %>% 
      filter(!is.na(!!sym(outcome)) & !is.na(!!sym(predictor)))
    if (is.factor(data_subset[[predictor]]) || is.character(data_subset[[predictor]])) {
      # Categorical predictor
      # cont_table <- table(data_subset[[predictor]], data_subset[[outcome]])
      chi_test <- chisq.test(data_subset[[outcome]],data_subset[[predictor]])
      
      results$chi_square[[predictor]] <- tidy(chi_test)
    } else {
      # Continuous predictor
       formula <- as.formula(paste(predictor, "~", outcome))
        model <- lm(formula, data = data_subset)
        anova_result <- Anova(model, type = "III")
        results$anova[[predictor]] <- tidy(anova_result)[2,]
      }
 #   }
  }
  
  return(results)
}

# Usage
payor_bivar <- bivariate_analysis(d_analysis, 
                              "payor_category_analysis", 
                              c(predictors[-1],
                                score_variables))

anova_table <- payor_bivar$anova %>%
  imap_dfr(~{
    .x %>%
      mutate(predictor = .y) %>%
      select(predictor, everything())
  }) %>%
  rename(
    sum_sq = sumsq,
    f_value = statistic
  ) %>%
  select(predictor, statistic=f_value, p.value)

chi_square_table <- payor_bivar$chi_square %>%
  imap_dfr(~{
    .x %>%
      mutate(predictor = .y) %>%
      select(predictor, everything())
  }) %>%
  select(predictor, statistic, p.value)

payor_bivariate_table <- rbind(chi_square_table,anova_table)

###############################################################################
# Step 4: Plotting of numeric predictors and outcomes
###############################################################################
# Function to iterate over predictors and plot numerics against outcomes
plot_linear_association <- function(data, outcome, predictors) {
  results <- list()
  logit <- function(pr) log(pr/(1-pr))
  for (predictor in predictors) {
    if (is.numeric(data[[predictor]])) {
      d <- data %>% 
        drop_na(outcome, predictor)
      loessfit <- predict(loess(d[[outcome]]~d[[predictor]]))
      pi <- pmax(pmin(loessfit,0.9999),0.0001)
      logitfitted <- logit(pi)
      o <- order(d[[predictor]])
      plot_data <- data.frame(x = d[[predictor]][o],
                                                   y = logitfitted[o])
      plot <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_line() +
        labs(x = predictor)
      results[[predictor]] <- plot
    }
  }
  return(results)
}

readmission_plots_test <- plot_linear_association(d_analysis,
                                                  outcome_variables[1],
                                                  c(score_variables, predictors))
readmission_plot_grid <- grid.arrange(grobs=readmission_plots_test, 
                                      top="30-Day Readmission")

ed_plots_test <- plot_linear_association(d_analysis,
                                         outcome_variables[2],
                                         c(score_variables, predictors))
ed_plot_grid <- grid.arrange(grobs=ed_plots_test, 
                             top="30-Day ed")

death_plots_test <- plot_linear_association(d_analysis,
                                            outcome_variables[3],
                                            c(score_variables, predictors))
death_plot_grid <- grid.arrange(grobs=death_plots_test, 
                                top="30-Day death")



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



# HOSPITAL model collinearity
# Subset data
hospital_subset <- d_analysis[, hospital_variables$variable]
hospital_numeric_vars <- sapply(hospital_subset, is.numeric)
hospital_numeric_subset <- hospital_subset[, hospital_numeric_vars]

# Calculate the correlation matrix for hospital_numeric_subset
hospital_correlation_matrix <- round(cor(hospital_numeric_subset, 
                                         use='pairwise.complete.obs'),3)



###############################################################################
# Step 5: Creating simple additive models
###############################################################################
# Based on the results of the bivariate analysis and scoring comps, averages
# and trend scores will be removed from the models

hospital_scores_1 <- c(score_variables[1:4])

news_scores_1 <- c(score_variables[6:9])

# Based on the curves, a smoothing term will be added to account for non-linearity
# in age in readmission models

predictors_1 <- c(predictors[-13], "ns(age_at_encounter, df = 3)")

model_formulas_1 <- list(
  hospital_read <- paste0("readmission_30days_recode_analysis ~ ",
                          paste(hospital_scores_1, collapse = " + "),
                          " + ",
                          paste(predictors_1, collapse = " + ")),
  # hospital_ed_visit <- paste0("ed_30days_recode_analysis ~ ",
  #                             paste(score_variables[1:5], collapse = " + "),
  #                             " + ",
  #                             paste(predictors, collapse = " + ")),
  hospital_death <- paste0("death_30_days_analysis ~ ",
                           paste(hospital_scores_1, collapse = " + "),
                           " + ",
                           paste(predictors, collapse = " + ")),
  news_read <- paste0("readmission_30days_recode_analysis ~ ",
                          paste(news_scores_1, collapse = " + "),
                      " + ",
                          paste(predictors_1, collapse = " + ")),
  # news_ed_visit <- paste0("ed_30days_recode_analysis ~ ",
  #                             paste(score_variables[6:10], collapse = " + "),
  #                         " + ",
  #                             paste(predictors, collapse = " + ")),
  news_death <- paste0("death_30_days_analysis ~ ",
                           paste(news_scores_1, collapse = " + "),
                       " + ",
                           paste(predictors, collapse = " + "))
)


fits_1 <- lapply(model_formulas_1, function(formula) {
  glm(formula, data = d_analysis, family = binomial)
})

fits_1_vifs <- lapply(fits_1, function(fit) {
  vif(fit)
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
drop1_table_1 <- do.call(cbind, lapply(drop1_list_1, 
                                       function(x) x$`Pr(>Chi)`[-1]))

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
                     "D-1 Score",
                     "D-2 Score",
                     "Admission Score",
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
                     "CMR Readmission Index",
                     "CMR Mortality Index",
                     "Age")

table_column_names <- c("HOSPITAL Readmission", 
                        # "HOSPITAL ED Visit",
                        "HOSPITAL Death",
                        "NEWS Readmission",
                        # "NEWS ED Visit",
                        "NEWS Death")

rownames(drop1_table_starred_1) <- table_row_names
colnames(drop1_table_starred_1) <- table_column_names

###############################################################################
# Applying drop1 resutls

hospital_scores_2 <- hospital_scores_1[c(1,4)]
news_scores_2 <- news_scores_1[c(1,4)]

model_formulas_2 <- list(
  hospital_read <- paste0("readmission_30days_recode_analysis ~ ",
                          paste(hospital_scores_2, collapse = " + "),
                          " + ",
                          paste(predictors_1, collapse = " + ")),
  # hospital_ed_visit <- paste0("ed_30days_recode_analysis ~ ",
  #                             paste(score_variables[2:5], collapse = " + "),
  #                             " + ",
  #                             paste(predictors, collapse = " + ")),
  hospital_death <- paste0("death_30_days_analysis ~ ",
                           paste(hospital_scores_2, collapse = " + "),
                           " + ",
                           paste(predictors, collapse = " + ")),
  news_read <- paste0("readmission_30days_recode_analysis ~ ",
                      paste(news_scores_2, collapse = " + "),
                      " + ",
                      paste(predictors_1, collapse = " + ")),
  # news_ed_visit <- paste0("ed_30days_recode_analysis ~ ",
  #                             paste(score_variables[6:20], collapse = " + "),
  #                         " + ",
  #                             paste(predictors, collapse = " + ")),
  news_death <- paste0("death_30_days_analysis ~ ",
                       paste(news_scores_2, collapse = " + "),
                       " + ",
                       paste(predictors, collapse = " + "))
)

fits_2 <- lapply(model_formulas_2, function(formula) {
  glm(formula, data = d_analysis, family = binomial)
})

fits_2_summary <- lapply(fits_2, function(fit) {
  tidy(fit)
})

fits_2_aic <- lapply(fits_2, function(fit) {
  fit$aic
})

drop1_list_2 <- lapply(fits_2, function(fit) {
  drop1(fit, test = "LRT")
})

# Creating a table of p-values from the Type III tests
drop1_table_2 <- do.call(cbind, lapply(drop1_list_2, 
                                       function(x) x$`Pr(>Chi)`[-1]))

# Including significance in tables
drop1_table_starred_2 <- apply(drop1_table_2, 2, function(x) {
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
                     "Admission Score",
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
                     "CMR Readmission Index",
                     "CMR Mortality Index",
                     "Age")


rownames(drop1_table_starred_2) <- table_row_names
colnames(drop1_table_starred_2) <- table_column_names



###############################################################################
# Third round of models

read_predictors <- predictors_1[-c(6,12)]
death_predictors <- predictors[-12]


model_formulas_3 <- list(
  hospital_read <- paste0("readmission_30days_recode_analysis ~ ",
                          paste(hospital_scores_2, collapse = " + "),
                          " + ",
                          paste(read_predictors, collapse = " + ")),

  hospital_death <- paste0("death_30_days_analysis ~ ",
                           paste(hospital_scores_2, collapse = " + "),
                           " + ",
                           paste(death_predictors, collapse = " + ")),
  news_read <- paste0("readmission_30days_recode_analysis ~ ",
                      paste(news_scores_2, collapse = " + "),
                      " + ",
                      paste(read_predictors, collapse = " + ")),

  news_death <- paste0("death_30_days_analysis ~ ",
                       paste(news_scores_2, collapse = " + "),
                       " + ",
                       paste(death_predictors, collapse = " + "))
)

fits_3 <- lapply(model_formulas_3, function(formula) {
  glm(formula, data = d_analysis, family = binomial)
})

fits_3_summary <- lapply(fits_3, function(fit) {
  tidy(fit)
})

fits_3_aic <- lapply(fits_3, function(fit) {
  fit$aic
})

drop1_list_3 <- lapply(fits_3, function(fit) {
  drop1(fit, test = "LRT")
})


drop1_table_read <- round(cbind(drop1_list_3[[1]]$`Pr(>Chi)`[-1],
                          drop1_list_3[[3]]$`Pr(>Chi)`[-1]),3)

# Including significance in tables
drop1_table_3_read_starred <- apply(drop1_table_read, 2, function(x) {
  case_when(
    x >= 0.1 ~ as.character(round(x, 3)),
    x >= 0.05 ~ paste0(round(x, 3), " -"),
    x >= 0.01 ~ paste0(round(x, 3), " **"),
    x >= 0.001 ~ paste0(round(x, 3), " ***"),
    TRUE ~ "<0.001 ****"
  )
})

rownames(drop1_table_3_read_starred) <- c(hospital_scores_2, read_predictors)
colnames(drop1_table_3_read_starred) <- c("HOSPITAL Readmission", "NEWS Readmission")

drop1_table_death <- round(cbind(drop1_list_3[[2]]$`Pr(>Chi)`[-1],
                                drop1_list_3[[4]]$`Pr(>Chi)`[-1]),3)

# Including significance in tables
drop1_table_3_death_starred <- apply(drop1_table_death, 2, function(x) {
  case_when(
    x >= 0.1 ~ as.character(round(x, 3)),
    x >= 0.05 ~ paste0(round(x, 3), " -"),
    x >= 0.01 ~ paste0(round(x, 3), " **"),
    x >= 0.001 ~ paste0(round(x, 3), " ***"),
    TRUE ~ "<0.001 ****"
  )
})

rownames(drop1_table_3_death_starred) <- c(hospital_scores_2, death_predictors)
colnames(drop1_table_3_death_starred) <- c("HOSPITAL death", "NEWS death")

###############################################################################
# Comparing the first and second models

names(fits_1_aic) <- table_column_names
names(fits_1_summary) <- table_column_names
names(fits_2_aic) <- table_column_names
names(fits_2_summary) <- table_column_names

fits_1_2_aic <- data.frame(
  AIC1 = unlist(fits_1_aic),
  AIC2 = unlist(fits_2_aic)
)

fits_1_2_aic$delta <- fits_1_2_aic$AIC2 - fits_1_2_aic$AIC1

###############################################################################
# Comparing the first and second models

names(fits_1_aic) <- table_column_names
names(fits_1_summary) <- table_column_names
names(fits_2_aic) <- table_column_names
names(fits_2_summary) <- table_column_names

fits_1_2_aic <- data.frame(
  AIC1 = unlist(fits_1_aic),
  AIC2 = unlist(fits_2_aic)
)

fits_1_2_aic$delta <- fits_1_2_aic$AIC2 - fits_1_2_aic$AIC1

###############################################################################
# Fitting the interactive models
hospital_scores_i <- hospital_scores_2
news_scores_i <- news_scores_2
read_predictors_i <- read_predictors
death_predictors_i <- death_predictors

model_formulas_i <- list(
  hospital_read <- paste0("readmission_30days_recode_analysis ~ ",
                          paste(c(hospital_scores_i[1],
                                  read_predictors_i[1])
                                  , collapse = " * "),
                          " + ",
                          paste(hospital_scores_i[-1]
                                , collapse = " + "),
                          " + ",
                          paste(read_predictors_i[-1], collapse = " + ")),
  
  hospital_death <- paste0("death_30_days_analysis ~ ",
                           paste(c(hospital_scores_i[1],
                                   death_predictors_i[1])
                                 , collapse = " * "),
                           " + ",
                           paste(hospital_scores_i[-1]
                                 , collapse = " + "),
                           " + ",
                           paste(death_predictors_i[-1], collapse = " + ")),
  
  news_read <- paste0("readmission_30days_recode_analysis ~ ",
                      paste(c(news_scores_i[1],
                              read_predictors_i[1])
                            , collapse = " * "),
                      " + ",
                      paste(news_scores_i[-1]
                            , collapse = " + "),
                      " + ",
                      paste(read_predictors_i[-1], collapse = " + ")),
  
  news_death <- paste0("death_30_days_analysis ~ ",
                       paste(c(news_scores_i[1],
                               death_predictors_i[1])
                             , collapse = " * "),
                       " + ",
                       paste(news_scores_i[-1]
                             , collapse = " + "),
                       " + ",
                       paste(death_predictors_i[-1], collapse = " + "))
  )

fits_i <- lapply(model_formulas_i, function(formula) {
  glm(formula, data = d_analysis, family = binomial)
})

fits_i_summary <- lapply(fits_i, function(fit) {
  tidy(fit)
})

fits_i_aic <- lapply(fits_i, function(fit) {
  fit$aic
})

drop1_list_i <- lapply(fits_i, function(fit) {
  drop1(fit, test = "LRT")
})

drop1_table_read_i <- round(cbind(drop1_list_i[[1]]$`Pr(>Chi)`[-1],
                                  drop1_list_i[[3]]$`Pr(>Chi)`[-1]),3)

# Including significance in tables
drop1_table_i_read_starred <- apply(drop1_table_read_i, 2, function(x) {
  case_when(
    x >= 0.1 ~ as.character(round(x, 3)),
    x >= 0.05 ~ paste0(round(x, 3), " -"),
    x >= 0.01 ~ paste0(round(x, 3), " **"),
    x >= 0.001 ~ paste0(round(x, 3), " ***"),
    TRUE ~ "<0.001 ****"
  )
})

rownames(drop1_table_i_read_starred) <- c(hospital_scores_2[-1], 
                                          read_predictors[-1], 
                                          "*")
colnames(drop1_table_i_read_starred) <- c("HOSPITAL Readmission", "NEWS Readmission")

drop1_table_death_i <- round(cbind(drop1_list_i[[2]]$`Pr(>Chi)`[-1],
                                   drop1_list_i[[4]]$`Pr(>Chi)`[-1]),3)

# Including significance in tables
drop1_table_3_death_starred_i <- apply(drop1_table_death_i, 2, function(x) {
  case_when(
    x >= 0.1 ~ as.character(round(x, 3)),
    x >= 0.05 ~ paste0(round(x, 3), " -"),
    x >= 0.01 ~ paste0(round(x, 3), " **"),
    x >= 0.001 ~ paste0(round(x, 3), " ***"),
    TRUE ~ "<0.001 ****"
  )
})

rownames(drop1_table_3_death_starred_i) <- c(hospital_scores_2[-1], 
                                             death_predictors[-1],
                                             "*")
colnames(drop1_table_3_death_starred_i) <- c("HOSPITAL death", "NEWS death")
