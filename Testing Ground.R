###################################################################
# Plotting pairwise comps
library(multcomp)
library(ggplot2)

# Step 1: Fit your logistic regression model
# Assuming your model is already fit and called 'model'
# model <- glm(outcome ~ categorical_var + other_predictors, data = your_data, family = binomial)

# Step 2: Perform pairwise comparisons using glht
comparisons <- glht(fits_1[[1]], linfct = mcp(payor_category_analysis = "Tukey"))

# Step 3: Get the summary of the comparisons
summary_comp <- summary(comparisons)

# Step 4: Extract the necessary information for plotting
plot_data <- data.frame(
  comparison = rownames(summary_comp$test$coefficients),
  estimate = summary_comp$test$coefficients,
  lower = summary_comp$test$coefficients - 1.96 * summary_comp$test$sigma,
  upper = summary_comp$test$coefficients + 1.96 * summary_comp$test$sigma
)

# Step 5: Create the plot
ggplot(plot_data, aes(x = comparison, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(x = "Pairwise Comparison", y = "Estimated Difference (log odds)",
       title = "Pairwise Comparisons of Categorical Variable") +
  theme_minimal()


############################################################################
# Hunting for interaction

library(broom)
library(purrr)
library(dplyr)

test_interactions <- function(base_formula, data, score_var, predictors) {
  # Function to add interaction term to formula
  add_interaction <- function(formula, var) {
    update(formula, paste0(". ~ . + ", score_var, ":", var))
  }
  
  # Convert base formula to formula object if it's a string
  if(is.character(base_formula)) base_formula <- as.formula(base_formula)
  
  # Iterate over predictors
  results <- map_dfr(predictors, function(pred) {
    # Add interaction term
    formula_with_interaction <- add_interaction(base_formula, pred)
    
    # Fit model
    model <- glm(formula_with_interaction, family = binomial, data = data)
    
    # Perform drop1 test
    drop1_result <- drop1(model, test = "Chisq")
    
    # Extract p-value for the interaction term
    interaction_term <- paste0(score_var, ":", pred)
    p_value <- drop1_result[interaction_term, "Pr(>Chi)"]
    
    # Return results
    tibble(
      predictor = pred,
      p_value = p_value
    )
  })
  
  # Sort results by p-value
  results %>% arrange(p_value)
}

# Apply to each model in fits_3
interaction_results <- map(names(fits_3), function(model_name) {
  cat("\nTesting interactions for", model_name, "model:\n")
  
  # Extract base formula and data
  base_formula <- formula(fits_3[[model_name]])
  data <- fits_3[[model_name]]$model
  
  # Determine the appropriate score variable and predictors
  if (grepl("HOSPITAL", model_name)) {
    score_var <- "discharge_HOSPITAL_score"
    predictors <- setdiff(names(data), c("readmission_30days_recode_analysis", "death_30_days_analysis", hospital_scores_2))
  } else {
    score_var <- "discharge_news_score"
    predictors <- setdiff(names(data), c("readmission_30days_recode_analysis", "death_30_days_analysis", news_scores_2))
  }
  
  # Test interactions
  results <- test_interactions(base_formula, data, score_var, predictors)
  
  print(results)
  results
})

names(interaction_results) <- names(fits_3)
