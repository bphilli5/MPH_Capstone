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
create_roc_plot <- function(obs, preds, title, n_comparisons = 3) {
  # Calculate Bonferroni-corrected confidence level
  conf_level <- 1 - (0.05 / n_comparisons)
  
  roc_obj <- roc(obs, preds)
  ci <- ci.se(roc_obj, specificities = seq(0, 1, 0.01), conf.level = conf_level)
  
  plot_data <- data.frame(
    FPR = 1 - as.numeric(rownames(ci)),
    TPR = ci[, 2],
    lower = ci[, 1],
    upper = ci[, 3]
  )
  
  ggplot(plot_data, aes(x = FPR, y = TPR)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue") +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = title,
         x = "False Positive Rate",
         y = "True Positive Rate") +
    annotate("text", x = 0.75, y = 0.25, 
             label = paste("AUC =", round(auc(roc_obj), 3))) +
    theme_minimal()
}

# Function to create calibration plots
create_calibration_plot <- function(obs, preds, title, n_groups = 10) {
  # Create a data frame with observed outcomes and predicted probabilities
  cal_data <- data.frame(obs = obs, pred = preds)
  
  # Sort the data by predicted probabilities and divide into groups
  cal_data <- cal_data[order(cal_data$pred),]
  cal_data$group <- cut(seq(nrow(cal_data)), breaks = n_groups, labels = FALSE)
  
  # Calculate mean predicted and observed probabilities for each group
  cal_summary <- cal_data %>%
    group_by(group) %>%
    summarise(
      mean_pred = mean(pred),
      mean_obs = mean(obs),
      n = n()
    )
  
  # Create the calibration plot
  ggplot(cal_summary, aes(x = mean_pred, y = mean_obs)) +
    geom_point(aes(size = n), alpha = 0.7) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    coord_equal() +
    xlim(0, 1) + ylim(0, 1) +
    labs(
      title = title,
      x = "Predicted Probability",
      y = "Observed Proportion",
      size = "Group Size"
    ) +
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
                    paste0(model_names[i], " (with interaction)"), n_comparisons = 3)
  
  # Without interaction
  roc_plots[[paste0(model_names[i], " (without interaction)")]] <- 
    create_roc_plot(results_without_interaction[[i]]$obs, results_without_interaction[[i]]$preds, 
                    paste0(model_names[i], " (without interaction)"), n_comparisons = 3)
}

# Arrange plots in a grid
ROC_plot_grid <- do.call(grid.arrange, c(roc_plots, ncol = 2))


# Create calibration plots
cal_plots <- list()

for (i in 1:4) {
  # With interaction
  cal_plots[[paste0(model_names[i], " (with interaction)")]] <- 
    create_calibration_plot(results_with_interaction[[i]]$obs, 
                            results_with_interaction[[i]]$preds, 
                            paste0(model_names[i], " (with interaction)"))
  
  # Without interaction
  cal_plots[[paste0(model_names[i], " (without interaction)")]] <- 
    create_calibration_plot(results_without_interaction[[i]]$obs, 
                            results_without_interaction[[i]]$preds, 
                            paste0(model_names[i], " (without interaction)"))
}

# Arrange calibration plots in a grid
cal_plot_grid <- do.call(grid.arrange, c(cal_plots, ncol = 2))


################################################################################
# Step 2: Subgroup analysis
################################################################################
# Define insurance categories to analyze
insurance_categories <- c("Commercial", "Medicare", "Medicaid", "Self-pay", "Other")

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


combine_rare_levels <- function(data, var, min_freq = 0.05) {
  freq <- table(data[[var]]) / nrow(data)
  levels_to_keep <- names(freq)[freq >= min_freq]
  data[[var]] <- factor(ifelse(data[[var]] %in% levels_to_keep, 
                               as.character(data[[var]]), 
                               "Other"))
  return(data)
}

# Apply this to relevant categorical variables before running the analysis
categorical_vars <- c("discharge_service_analysis", "race_category_analysis", 
                      "language_category_analysis", "ICU_category_analysis", 
                      "dc_disp_category_analysis", "patient_class_analysis", 
                      "facility_name_analysis")

k_subset <- 5
# Function to perform analysis for a specific insurance category
analyze_insurance_category <- function(insurance_category, d_analysis, model_formulas) {
  # Filter data for the specific insurance category
  d_filtered <- d_analysis %>% filter(payor_category_analysis == insurance_category)
  for (var in categorical_vars) {
    d_filtered <- combine_rare_levels(d_filtered, var)
  }
  # Apply the function to each of your models
  results <- list()
  
  for (model_name in seq_along(model_formulas)) {
    k <- k_subset
    success <- FALSE
    
    while (k >= 2 && !success) {
      cat(sprintf("Insurance category: %s, Model: %s, k: %d\n", insurance_category, names(model_formulas)[model_name], k))
      
      result <- tryCatch({
        cv_performance_extended(as.formula(model_formulas[[model_name]]), d_filtered, k = k)
      }, error = function(e) {
        cat(sprintf("Error occurred with k = %d. Error message: %s\n", k, e$message))
        return(NULL)
      })
      
      if (!is.null(result)) {
        results[[model_name]] <- result
        success <- TRUE
        cat(sprintf("Successfully completed analysis for %s with k = %d\n", names(model_formulas)[model_name], k))
      } else {
        k <- k - 1
        cat(sprintf("Reducing k to %d...\n", k))
      }
    }
    
    if (!success) {
      cat(sprintf("Failed to run analysis for %s with insurance category %s\n", names(model_formulas)[model_name], insurance_category))
      results[[model_name]] <- NULL
    }
  }
  
  # Remove NULL results
  results <- results[!sapply(results, is.null)]
  
  # If all models failed, return NULL
  if (length(results) == 0) {
    cat(sprintf("All models failed for insurance category %s\n", insurance_category))
    return(NULL)
  }
  
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
      create_roc_plot(results[[i]]$obs, results[[i]]$preds, model_names[i], n_comparisons = 3)
  }

  # Arrange plots in a grid
  plot_grid <- grid.arrange(
    grobs = roc_plots,
    ncol = 2,
    top = textGrob(paste(insurance_category, "Insurance"), gp = gpar(fontsize = 20, fontface = "bold"))
  )

  # Create calibration plots
  cal_plots <- list()
  for (i in 1:4) {
    cal_plots[[model_names[i]]] <-
      create_calibration_plot(results[[i]]$obs, results[[i]]$preds, model_names[i])
  }

  # Arrange calibration plots in a grid
  cal_plot_grid <- grid.arrange(
    grobs = cal_plots,
    ncol = 2,
    top = textGrob(paste(insurance_category, "Insurance - Calibration"),
                   gp = gpar(fontsize = 20, fontface = "bold"))
  )
  
  # Create a list of observed values and predictions for each model
  model_results <- lapply(seq_along(model_names), function(i) {
    list(
      obs = results[[i]]$obs,
      preds = results[[i]]$preds
    )
  })
  names(model_results) <- model_names
  
  return(list(summary_table = summary_table, 
              roc_plot_grid = plot_grid,
              cal_plot_grid = cal_plot_grid,
              model_results = model_results))
}

# Perform analysis for each insurance category
results_by_insurance <- lapply(insurance_categories, function(insurance) {
  analyze_insurance_category(insurance, d_analysis, model_formulas_stratified)
})

names(results_by_insurance) <- insurance_categories

# Print summary tables and store plot grids for each insurance category
roc_plot_grids <- list()
cal_plot_grids <- list()
for (insurance in insurance_categories) {
  print(paste("Summary for", insurance, "Insurance:"))
  print(results_by_insurance[[insurance]]$summary_table)
  roc_plot_grids[[insurance]] <- results_by_insurance[[insurance]]$roc_plot_grid
  cal_plot_grids[[insurance]] <- results_by_insurance[[insurance]]$cal_plot_grid
}

################################################################################
# Step 3: Comparing AUROCs across insurance categories
################################################################################

compare_aurocs <- function(results_by_insurance, model_name) {
  insurance_categories <- names(results_by_insurance)
  n_categories <- length(insurance_categories)
  comparisons <- combn(n_categories, 2)
  
  results <- list()
  
  for (i in 1:ncol(comparisons)) {
    cat1 <- insurance_categories[comparisons[1, i]]
    cat2 <- insurance_categories[comparisons[2, i]]
    
    roc1 <- roc(results_by_insurance[[cat1]]$model_results[[model_name]]$obs, 
                results_by_insurance[[cat1]]$model_results[[model_name]]$preds)
    roc2 <- roc(results_by_insurance[[cat2]]$model_results[[model_name]]$obs, 
                results_by_insurance[[cat2]]$model_results[[model_name]]$preds)
    
    test_result <- roc.test(roc1, roc2, method = "delong")
    
    results[[paste(cat1, "vs", cat2)]] <- list(
      p_value = test_result$p.value,
      auc_diff = as.numeric(test_result$estimate[1] - test_result$estimate[2])
    )
  }
  
  return(results)
}

# Apply this function to each model
model_names <- c("HOSPITAL - Death", "NEWS2 - Death")

auroc_comparisons <- lapply(model_names, function(model) {
  compare_aurocs(results_by_insurance, model)
})
names(auroc_comparisons) <- model_names

create_comparison_table <- function(auroc_comparisons) {
  comparison_table <- data.frame()
  
  for (model in names(auroc_comparisons)) {
    for (comparison in names(auroc_comparisons[[model]])) {
      row <- data.frame(
        Model = model,
        Comparison = comparison,
        AUC_Difference = abs(auroc_comparisons[[model]][[comparison]]$auc_diff),
        P_Value = auroc_comparisons[[model]][[comparison]]$p_value
      )
      comparison_table <- rbind(comparison_table, row)
    }
  }
  
  comparison_table$Significant <- ifelse(comparison_table$P_Value < 0.05, "*", "")
  
  return(comparison_table)
}

comparison_summary <- create_comparison_table(auroc_comparisons)
print(comparison_summary)
