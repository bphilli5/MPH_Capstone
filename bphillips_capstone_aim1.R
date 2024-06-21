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
score_variable_factor <- names(d_analysis)[28:35]
score_variable_quantile <- names(d_analysis)[36:43]
score_variable_averages <- names(d_analysis)[44:45]
predictors <- names(d_analysis)[14:27]
# Removing LOS based on clinical irrelevance of results
predictors <- predictors[-12]
hospital_variables <- data.frame(
  variable = c(score_variables[1:5],
               predictors)
)
news_variables <- data.frame(
  variable = c(score_variables[6:10],
               predictors)
)

hospital_scores <- c(score_variables[1:5],
                     score_variable_factor[1:4],
                     score_variable_quantile[1:4],
                     score_variable_averages[1])

news_scores <- c(score_variables[6:10],
                 score_variable_factor[5:8],
                 score_variable_quantile[5:8],
                 score_variable_averages[2])

read_predictors <- predictors
ed_predictors <- predictors
death_predictors <- predictors


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

# Create LRT tables and view
readmission_table_lrt <- do.call(rbind, readmission_results$lrt)

view(tmp <- readmission_table_lrt %>% 
       mutate(p.value = round(p.value,3)))

# Based on the results of the bivariate analysis, ICU_category can be removed
# from models of readmission risk
read_predictors <- read_predictors[-6]

ed_table_lrt <- do.call(rbind, ed_results$lrt)

view(tmp <- ed_table_lrt %>% 
       mutate(p.value = round(p.value,3)))
# Removing several predictors based on results
ed_predictors <- ed_predictors[-c(19,26)]

# Based on the results of bivariate analysis, NEWS2 doe not appear to be a 
# significant predictor of ED readmission
news_scores_ed <- NA

death_table_lrt <- do.call(rbind, death_results$lrt)

view(tmp <- death_table_lrt %>% 
       mutate(p.value = round(p.value,3)))

# Removing ethnicity as a predictor
death_predictors <- death_predictors[-4]

###############################################################################
# Step 5: Comparing methods of modeling scores
###############################################################################

readmission_score_comps <- cbind(readmission_table_lrt[c(1,2,3,4,6,7,8,9),],
                                 readmission_table_lrt[24:31,],
                                 readmission_table_lrt[32:39,],
                                 readmission_table_lrt[c(5,10,40,41),]
)

col_names_comps <- c("Nums", "Nums Dev", "Nums p",
                     "Fac", "Fac Dev", "Fac p",
                     "Quants", "Quants Dev", "Quants p",
                     "Avgs", "Avgs Dev", "Avgs p")

colnames(readmission_score_comps) <- col_names_comps

readmission_score_comps <- readmission_score_comps %>% 
  mutate(across(c(Nums,
                  Fac,
                  Quants,
                  Avgs),
                .fns = ~sub(".*~", "", .))
         )
# For HOSPITAL scores, the numeric version of the variable produces a better fit
# in all scenarios.
hospital_scores_read <- hospital_scores[-c(6:13)]

# For NEWS2 scores, the numeric version of the variable produces a better fit
# or is comparable

news_scores_read <- news_scores[-c(6:13)]

ed_score_comps <- cbind(ed_table_lrt[c(1,2,3,4,6,7,8,9),],
                          ed_table_lrt[24:31,],
                          ed_table_lrt[32:39,],
                        ed_table_lrt[c(5,10,40,41),])

colnames(ed_score_comps) <- col_names_comps

ed_score_comps <- ed_score_comps %>% 
  mutate(across(c(Nums,
                  Fac,
                  Quants),
                .fns = ~sub(".*~", "", .))
  )

# For HOSPITAL scores, the numeric version of the variable produces a better
# fit in all scenarios or is comparable

hospital_scores_ed <- hospital_scores[-c(6:13)]

# No changes for NEWS scores for ed

death_score_comps <- cbind(death_table_lrt[c(1,2,3,4,6,7,8,9),],
                        death_table_lrt[24:31,],
                        death_table_lrt[32:39,],
                        death_table_lrt[c(5,10,40,41),])

colnames(death_score_comps) <- col_names_comps

death_score_comps <- death_score_comps %>% 
  mutate(across(c(Nums,
                  Fac,
                  Quants),
                .fns = ~sub(".*~", "", .))
  )

# For HOSPITAL scores, the numeric version of the variable produces a better
# fit in all scenarios except admission
hospital_scores_death <- hospital_scores[-c(4,6:8,10:13)]
# For NEWS2 scores, the factor version of the variable produces a better fit in
# all scenarios except admission
news_scores_death <- news_scores[-c(1:3,9:13)]

###############################################################################
# Step 5: Bivariate analysis with payor category
###############################################################################

bivariate_analysis <- function(data, outcome, predictors) {
  results <- list()
  
  for (predictor in predictors) {
    data_subset <- data %>% 
      filter(!is.na(!!sym(outcome)) & !is.na(!!sym(predictor)))
    print(predictor)
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
shuggie <- bivariate_analysis(d_analysis, 
                              "payor_category_analysis", 
                              c(predictors[-1],
                                score_variables))

anova_table <- shuggie$anova %>%
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

chi_square_table <- shuggie$chi_square %>%
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

# Based on the curves, a smoothing term will be added to account for non-linearity
# in age

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
                          paste(hospital_scores_read, collapse = " + "),
                          " + ",
                          paste(read_predictors, collapse = " + ")),
  hospital_ed_visit <- paste0("ed_30days_recode_analysis ~ ",
                              paste(hospital_scores_ed, collapse = " + "),
                              " + ",
                              paste(ed_predictors, collapse = " + ")),
  hospital_death <- paste0("death_30_days_analysis ~ ",
                           paste(hospital_scores_death, collapse = " + "),
                           " + ",
                           paste(death_predictors, collapse = " + ")),
  news_read <- paste0("readmission_30days_recode_analysis ~ ",
                          paste(news_scores_read, collapse = " + "),
                      " + ",
                          paste(read_predictors, collapse = " + ")),
  news_ed_visit <- paste0("ed_30days_recode_analysis ~ ",
                              paste(news_scores_ed, collapse = " + "),
                          " + ",
                              paste(ed_predictors, collapse = " + ")),
  news_death <- paste0("death_30_days_analysis ~ ",
                           paste(news_scores_death, collapse = " + "),
                       " + ",
                           paste(death_predictors, collapse = " + "))
)

fits_1 <- lapply(model_formulas, function(formula) {
  glm(formula, data = d_analysis, family = binomial)
})

###############################################################################
# Step 6: Fitting the interactive models
###############################################################################

model_formulas_i <- list(
  hospital_read <- paste0(
    "readmission_30days_recode_analysis ~ discharge_HOSPITAL_score * ",
    paste(hospital_variables$variable[2:length(hospital_variables$variable)], collapse = " + ")),
  hospital_ed_visit <- paste0(
    "ed_30days_recode_analysis ~ discharge_HOSPITAL_score * ",
    paste(hospital_variables$variable[2:length(hospital_variables$variable)], collapse = " + ")),
  hospital_death <- paste0(
    "death_30_days_analysis ~ discharge_HOSPITAL_score * ",
    paste(hospital_variables$variable[2:length(hospital_variables$variable)], collapse = " + ")),
  news_readmission = paste0(
    "readmission_30days_recode_analysis ~ discharge_HOSPITAL_score * ",
    paste(news_variables$variable[2:length(hospital_variables$variable)], collapse = " + ")),
  news_ed_visit = paste0(
    "ed_30days_recode_analysis ~ discharge_HOSPITAL_score * ",
    paste(news_variables$variable[2:length(hospital_variables$variable)], collapse = " + ")),
  news_death = paste0(
    "death_30_days_analysis ~ discharge_HOSPITAL_score * ",
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
  tidy(drop1(fit, test = "LRT"))[-1,c(1,2,4,6)]
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


read_predictors <- c(read_predictors, "s(age_at_encounter)")
ed_predictors <- c(ed_predictors, "s(age_at_encounter)")
death_predictors <- c(death_predictors, "s(age_at_encounter)")
