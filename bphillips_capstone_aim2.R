###############################################################################
#> bphillips_capstone_aim2
#> Analysis related to the second aim: assessing the predictive performance of
#> the models and subgroup analysis.
###############################################################################

#> Step 1: Assessing the predictive performance of the four models
#> Step 2: Defining independent variables for HOSPITAL and NEWS2 models 
#> Step 3: Bivariate analysis

#################################################################################
# Step 1: Assessing the predictive performance of the four models
#################################################################################

# Modify the cv_performance_extended function to include these calculations
cv_performance_extended <- function(formula, data, k = 10) {
  outcome_var <- all.vars(formula)[1]
  all_vars <- all.vars(formula)
  complete_data <- data[complete.cases(data[, all_vars]), ]
  
  set.seed(123)
  folds <- createFolds(complete_data[[outcome_var]], k = k, list = TRUE, returnTrain = FALSE)
  
  auroc <- numeric(k)
  brier <- numeric(k)
  all_preds <- numeric(nrow(complete_data))
  all_obs <- complete_data[[outcome_var]]
  
  for (i in 1:k) {
    train <- complete_data[-folds[[i]], ]
    test <- complete_data[folds[[i]], ]
    
    model <- glm(formula, data = train, family = binomial)
    pred_prob <- predict(model, newdata = test, type = "response")
    
    all_preds[folds[[i]]] <- pred_prob
    auroc[i] <- auc(test[[outcome_var]], pred_prob)
    brier[i] <- mean((pred_prob - test[[outcome_var]])^2)
  }
  
  roc_obj <- roc(all_obs, all_preds)
  optimal_threshold <- coords(roc_obj, "best", best.method = "youden")$threshold
  
  predicted_class <- factor(ifelse(all_preds >= optimal_threshold, 1, 0), levels = c(0, 1))
  cm <- confusionMatrix(predicted_class, factor(all_obs, levels = c(0, 1)))
  
  return(list(
    mean_auroc = mean(auroc),
    mean_brier = mean(brier),
    sd_auroc = sd(auroc),
    sd_brier = sd(brier),
    preds = all_preds,
    obs = all_obs,
    optimal_threshold = optimal_threshold,
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"],
    ppv = cm$byClass["Pos Pred Value"],
    npv = cm$byClass["Neg Pred Value"],
    accuracy = cm$overall["Accuracy"]
  ))
}

# Function to create ROC plot
create_roc_plot <- function(obs, preds, title) {
  roc_obj <- roc(obs, preds)
  plot_data <- data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities
  )
  
  ggplot(plot_data, aes(x = FPR, y = TPR)) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = title,
         x = "False Positive Rate",
         y = "True Positive Rate") +
    annotate("text", x = 0.75, y = 0.25, 
             label = paste("AUC =", round(auc(roc_obj), 3))) +
    theme_minimal()
}

# Apply the function to each of your models
model_names <- c("HOSPITAL - Readmission", "HOSPITAL - Death", 
                 "NEWS2 - Readmission", "NEWS2 - Death")

results_with_interaction <- lapply(model_formulas_i, function(formula_str) {
  cv_performance_extended(as.formula(formula_str), d_analysis)
})

results_without_interaction <- lapply(model_formulas_3, function(formula_str) {
  cv_performance_extended(as.formula(formula_str), d_analysis)
})

# Create summary table with additional metrics
delong_tests <- lapply(1:4, function(i) {
  roc1 <- roc(results_with_interaction[[i]]$obs, results_with_interaction[[i]]$preds)
  roc2 <- roc(results_without_interaction[[i]]$obs, results_without_interaction[[i]]$preds)
  roc.test(roc1, roc2, method = "delong")
})

# Create summary table with additional metrics including DeLong's p-value
ROC_summary_table <- data.frame(
  Model = rep(model_names, times = 2),
  Interaction = rep(c("With", "Without"), each = 4),
  AUROC = c(sapply(results_with_interaction, function(x) round(x$mean_auroc, 3)),
            sapply(results_without_interaction, function(x) round(x$mean_auroc, 3))),
  Brier = c(sapply(results_with_interaction, function(x) round(x$mean_brier, 3)),
            sapply(results_without_interaction, function(x) round(x$mean_brier, 3))),
  Threshold = c(sapply(results_with_interaction, function(x) round(x$optimal_threshold, 3)),
                sapply(results_without_interaction, function(x) round(x$optimal_threshold, 3))),
  Sensitivity = c(sapply(results_with_interaction, function(x) round(x$sensitivity, 3)),
                  sapply(results_without_interaction, function(x) round(x$sensitivity, 3))),
  Specificity = c(sapply(results_with_interaction, function(x) round(x$specificity, 3)),
                  sapply(results_without_interaction, function(x) round(x$specificity, 3))),
  PPV = c(sapply(results_with_interaction, function(x) round(x$ppv, 3)),
          sapply(results_without_interaction, function(x) round(x$ppv, 3))),
  NPV = c(sapply(results_with_interaction, function(x) round(x$npv, 3)),
          sapply(results_without_interaction, function(x) round(x$npv, 3))),
  Accuracy = c(sapply(results_with_interaction, function(x) round(x$accuracy, 3)),
               sapply(results_without_interaction, function(x) round(x$accuracy, 3)))
)

# Add DeLong's p-value to the summary table
ROC_summary_table$DeLong_p <- rep(sapply(delong_tests, function(x) round(x$p.value, 4)), times = 2)

ROC_summary_table <- ROC_summary_table[order(ROC_summary_table$Model),]

# Create and arrange ROC plots (keep this part as before)
roc_plots <- list()

for (i in 1:4) {
  # With interaction
  roc_plots[[paste0(model_names[i], " (with interaction)")]] <- 
    create_roc_plot(results_with_interaction[[i]]$obs, results_with_interaction[[i]]$preds, 
                    paste0(model_names[i], " (with interaction)"))
  
  # Without interaction
  roc_plots[[paste0(model_names[i], " (without interaction)")]] <- 
    create_roc_plot(results_without_interaction[[i]]$obs, results_without_interaction[[i]]$preds, 
                    paste0(model_names[i], " (without interaction)"))
}

# Arrange plots in a grid
ROC_plot_grid <- do.call(grid.arrange, c(roc_plots, ncol = 2))


################################################################################
# Step 2: Subgroup analysis
################################################################################
# Define insurance categories to analyze
insurance_categories <- c("Commercial", "Medicare", "Medicaid")

# Redefine model formulas without payor status
model_formulas_stratified <- list(
  hospital_read = paste0("readmission_30days_recode_analysis ~ ",
                         paste(hospital_scores_2, collapse = " + "),
                         " + ",
                         paste(read_predictors[-1], collapse = " + ")),
  hospital_death = paste0("death_30_days_analysis ~ ",
                          paste(hospital_scores_2, collapse = " + "),
                          " + ",
                          paste(death_predictors[-1], collapse = " + ")),
  news_read = paste0("readmission_30days_recode_analysis ~ ",
                     paste(news_scores_2, collapse = " + "),
                     " + ",
                     paste(read_predictors[-1], collapse = " + ")),
  news_death = paste0("death_30_days_analysis ~ ",
                      paste(news_scores_2, collapse = " + "),
                      " + ",
                      paste(death_predictors[-1], collapse = " + "))
)

# Function to perform analysis for a specific insurance category
analyze_insurance_category <- function(insurance_category, d_analysis, model_formulas) {
  # Filter data for the specific insurance category
  d_filtered <- d_analysis %>% filter(payor_category_analysis == insurance_category)
  
  # Apply the function to each of your models
  model_names <- c("HOSPITAL - Readmission", "HOSPITAL - Death", 
                   "NEWS2 - Readmission", "NEWS2 - Death")
  
  results <- lapply(model_formulas, function(formula_str) {
    cv_performance_extended(as.formula(formula_str), d_filtered, k=8)
  })
  
  # Create summary table
  summary_table <- data.frame(
    Model = model_names,
    AUROC = sapply(results, function(x) round(x$mean_auroc, 3)),
    Brier = sapply(results, function(x) round(x$mean_brier, 3)),
    Threshold = sapply(results, function(x) round(x$optimal_threshold, 3)),
    Sensitivity = sapply(results, function(x) round(x$sensitivity, 3)),
    Specificity = sapply(results, function(x) round(x$specificity, 3)),
    PPV = sapply(results, function(x) round(x$ppv, 3)),
    NPV = sapply(results, function(x) round(x$npv, 3)),
    Accuracy = sapply(results, function(x) round(x$accuracy, 3))
  )
  
  # Create ROC plots
  roc_plots <- list()
  for (i in 1:4) {
    roc_plots[[model_names[i]]] <- 
      create_roc_plot(results[[i]]$obs, results[[i]]$preds, model_names[i])
  }
  
  # Arrange plots in a grid
  plot_grid <- grid.arrange(
    grobs = roc_plots,
    ncol = 2,
    top = textGrob(paste(insurance_category, "Insurance"), gp = gpar(fontsize = 20, fontface = "bold"))
  )
  
  return(list(summary_table = summary_table, plot_grid = plot_grid))
}

# Perform analysis for each insurance category
results_by_insurance <- lapply(insurance_categories, function(insurance) {
  analyze_insurance_category(insurance, d_analysis, model_formulas_stratified)
})
names(results_by_insurance) <- insurance_categories

# Print summary tables and store plot grids for each insurance category
plot_grids <- list()
for (insurance in insurance_categories) {
  plot_grids[[insurance]] <- results_by_insurance[[insurance]]$plot_grid
}
