ggplot(d_analysis, aes(x = discharge_news_score)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Distribution of Hospital Trend", x = "Value", y = "Density")

ggplot(d_analysis, aes(x = news_trend)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = .5, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Distribution of News Trend", x = "Value", y = "Density")


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




############################################################################################################
# Creating second set of models
names(fits_2) <- c("HOSPITAL Readmission", "HOSPITAL Mortality", "NEWS Readmission", "NEWS Mortality")
names(drop1_list_2) <- names(fits_1)

get_significant_predictors <- function(drop1_result, significance_level = 0.05) {
  significant_predictors <- rownames(drop1_result)[drop1_result$`Pr(>Chi)` < significance_level]
  return(significant_predictors)
}

model_formulas_2 <- lapply(names(fits_1), function(model_name) {
  significant_predictors <- get_significant_predictors(drop1_list_1[[model_name]])
  
  outcome <- strsplit(model_formulas_1[[model_name]], "~")[[1]][1]
  predictors <- paste(significant_predictors, collapse = " + ")
  
  paste0(outcome, "~ ", predictors)
})
names(model_formulas_2) <- names(fits_1)

#############################################################################
# Drop one talbes with diff lengths

drop1_table_2 <- do.call(cbind, lapply(drop1_list_2, function(x) {
  # Get all unique variable names across all models
  all_vars <- unique(unlist(lapply(drop1_list_2, function(m) rownames(m))))
  
  # Create a named vector with NAs for all variables
  result <- setNames(rep(NA, length(all_vars)), all_vars)
  
  # Fill in the available p-values
  available_vars <- rownames(x)[-1]  # Exclude the first row (usually the null model)
  result[available_vars] <- x$`Pr(>Chi)`[-1]
}))
