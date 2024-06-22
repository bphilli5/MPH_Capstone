ggplot(d_analysis, aes(x = hospital_trend)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = .5, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Distribution of Hospital Trend", x = "Value", y = "Density")

ggplot(d_analysis, aes(x = news_trend)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = .5, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Distribution of News Trend", x = "Value", y = "Density")


# Testing score variables
readmission_plots_test <- plot_linear_association(d_analysis,
                                             outcome_variables[1],
                                             c(score_variables, predictors))
readmission_plot_grid <- grid.arrange(grobs=readmission_plots_test, 
                                      top="30-Day Readmission")


ggplot(d_analysis, aes(x = CMR_Index_Readmission)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Distribution of CMR_Index_Readmission", x = "Value", y = "Density")

ggplot(d_analysis, aes(x = CMR_Index_Mortality)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Distribution of CMR_Index_Mortality", x = "Value", y = "Density")

data <- d_analysis
outcome <- "readmission_30days_recode_analysis"
predictor <- "discharge_HOSPITAL_score"
plot_linear_association <- function(data, outcome, predictors, n_points = 500) {
  results <- list()
  logit <- function(pr) log(pr/(1-pr))
  
  for (predictor in predictors) {
    if (is.numeric(data[[predictor]])) {
      d <- data %>% 
        drop_na(outcome, predictor)
      
      # Create a sequence of n_points between min and max of predictor
      pred_seq <- seq(min(d[[predictor]]), max(d[[predictor]]), length.out = n_points)
      
      # Fit LOESS model
      loess_model <- loess(d[[outcome]] ~ d[[predictor]])
      
      # Predict using the sequence
      newdata <- data.frame(temp = pred_seq)
      names(newdata) <- predictor
      loessfit <- predict(loess_model, newdata = newdata)
      loessfit <- predict(loess_model, newdata = data.frame(predictor = pred_seq))
      
      pi <- pmax(pmin(loessfit, 0.9999), 0.0001)
      logitfitted <- logit(pi)
      
      plot_data <- data.frame(x = pred_seq, y = logitfitted)
      
      plot <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_line() +
        labs(x = predictor, y = "Logit(Outcome)")
      
      results[[predictor]] <- plot
    }
  }
  return(results)
}


summary_to_df <- function(summary_object, fit_name) {
  bind_rows(summary_object, .id = "outcome") %>%
    mutate(fit = fit_name)
}

# Convert fits_1_summary to data frame
df1 <- summary_to_df(fits_1_summary, "fit1")

# Convert fits_2_summary to data frame (assuming you have this object)
df2 <- summary_to_df(fits_2_summary, "fit2")

# Combine the data frames
combined_df <- bind_rows(df1, df2)

# Reshape and calculate deltas
final_df <- combined_df %>%
  pivot_wider(
    id_cols = c(outcome, term),
    names_from = fit,
    values_from = c(estimate, std.error),
    names_sep = "_"
  ) %>%
  mutate(
    delta_estimate = estimate_fit1 - estimate_fit2,
    delta_std.error = std.error_fit1 - std.error_fit2
  )

fits_1_summary_filtered <- lapply(fits_1_summary, function(fit) {
  fit[c(2,9:13),1:3]
})

fits_1_summary_renamed <- imap(fits_1_summary_filtered, function(tibble, name) {
  name_cleaned <- str_to_upper(str_extract_all(name, "\\b\\w")[[1]]) %>% paste(collapse = "_")
  tibble %>%
    rename_with(~ paste(name_cleaned, .x, sep = "_"), -term)
})

fits_1_coeff_table <- reduce(fits_1_summary_renamed,left_join, by = "term")

pivot_tibble <- fits_1_coeff_table %>% 
  pivot_longer(
    cols = -term,
    names_to = c("model", ".value"),
    names_pattern = "(.+)_(.+)"
  )
names(pivot_tibble) <- c("term", "model", "fit1_est", "fit1_se")


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