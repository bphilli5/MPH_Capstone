library(caret)
library(pROC)
library(splines)  # for the ns() function

# Function to perform cross-validation for a specific model
cv_performance <- function(formula, data, k = 10) {
  # Extract outcome variable name from the formula
  outcome_var <- all.vars(formula)[1]
  
  # Get all variables in the formula
  all_vars <- all.vars(formula)
  
  # Remove rows with missing data for the variables in this model
  complete_data <- data[complete.cases(data[, all_vars]), ]
  
  # Create folds based on the outcome variable
  set.seed(123)  # for reproducibility
  folds <- createFolds(complete_data[[outcome_var]], k = k, list = TRUE, returnTrain = FALSE)
  
  auroc <- numeric(k)
  brier <- numeric(k)
  
  for (i in 1:k) {
    train <- complete_data[-folds[[i]], ]
    test <- complete_data[folds[[i]], ]
    
    # Fit model
    model <- glm(formula, data = train, family = binomial)
    
    # Make predictions
    pred_prob <- predict(model, newdata = test, type = "response")
    
    # Calculate performance metrics
    auroc[i] <- auc(test[[outcome_var]], pred_prob)
    brier[i] <- mean((pred_prob - test[[outcome_var]])^2)
  }
  
  return(list(mean_auroc = mean(auroc), mean_brier = mean(brier),
              sd_auroc = sd(auroc), sd_brier = sd(brier)))
}

# Apply the function to each of your four models
results <- lapply(model_formulas_i, function(formula_str) {
  cv_performance(as.formula(formula_str), d_analysis)
})

# Print results
model_names <- c("HOSPITAL - Readmission", "HOSPITAL - Death", 
                 "NEWS2 - Readmission", "NEWS2 - Death")

print_results <- function(model_name, results) {
  cat(model_name, ":\n")
  cat("Mean AUROC (SD):", round(results$mean_auroc, 3), "(", round(results$sd_auroc, 3), ")\n")
  cat("Mean Brier Score (SD):", round(results$mean_brier, 3), "(", round(results$sd_brier, 3), ")\n\n")
}

for (i in 1:4) {
  print_results(model_names[i], results[[i]])
}

# To assess the importance of insurance status, create versions without the interaction term
remove_insurance_interaction <- function(formula_str) {
  formula <- as.formula(formula_str)
  terms <- terms(formula)
  new_terms <- terms(update(terms, . ~ . - discharge_HOSPITAL_score:payor_category_analysis 
                            - discharge_news_score:payor_category_analysis))
  as.formula(paste(deparse(formula[[2]]), "~", paste(attr(new_terms, "term.labels"), collapse = " + ")))
}

model_formulas_no_interaction <- lapply(model_formulas_i, remove_insurance_interaction)

results_no_interaction <- lapply(model_formulas_no_interaction, function(formula) {
  cv_performance(formula, d_analysis)
})

# Print results for models without interaction
for (i in 1:4) {
  print_results(paste(model_names[i], "(without insurance interaction)"), results_no_interaction[[i]])
}

# Statistical comparison of models with and without insurance interaction
for (i in 1:4) {
  cat("Comparing", model_names[i], "with and without insurance interaction:\n")
  
  # Function to get fold-specific AUROCs
  get_fold_aurocs <- function(formula, data, k = 10) {
    outcome_var <- all.vars(formula)[1]
    all_vars <- all.vars(formula)
    complete_data <- data[complete.cases(data[, all_vars]), ]
    set.seed(123)
    folds <- createFolds(complete_data[[outcome_var]], k = k, list = TRUE, returnTrain = FALSE)
    
    aurocs <- numeric(k)
    
    for (j in 1:k) {
      train <- complete_data[-folds[[j]], ]
      test <- complete_data[folds[[j]], ]
      model <- glm(formula, data = train, family = binomial)
      pred_prob <- predict(model, newdata = test, type = "response")
      aurocs[j] <- auc(test[[outcome_var]], pred_prob)
    }
    
    return(aurocs)
  }
  
  aurocs_with <- get_fold_aurocs(as.formula(model_formulas_i[[i]]), d_analysis)
  aurocs_without <- get_fold_aurocs(model_formulas_no_interaction[[i]], d_analysis)
  
  t_test_result <- t.test(aurocs_with, aurocs_without, paired = TRUE)
  print(t_test_result)
  cat("\n")
}
