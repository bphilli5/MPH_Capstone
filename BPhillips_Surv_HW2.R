# Survival Analysis HW2
# Brendan Phillips

# Survival times for males and females
males <- c(1, 3, 4, 10, 12, 18)
females <- c(1, 3, 6, 10, 11, 12)

# Censoring indicators (0 = censored, 1 = event)
males_censor <- c(1, 1, 0, 1, 1, 1)
females_censor <- c(1, 0, 1, 1, 1, 1)

# 1. Kaplan-Meier estimates
library(survival)

km_males <- survfit(Surv(males, males_censor) ~ 1)
km_females <- survfit(Surv(females, females_censor) ~ 1)

# Print the Kaplan-Meier estimates
summary(km_males)
summary(km_females)

# Plot the estimated survivor functions
plot(km_males, conf.int = FALSE, col = "blue", xlab = "Time", ylab = "Survival Probability")
lines(km_females, conf.int = FALSE, col = "red")
legend("topright", c("Males", "Females"), col = c("blue", "red"), lty = 1)

# 2. Confidence intervals using Greenwood's formula
t1 <- 1
t2 <- 3

# For males
ci_males_t1 <- survfit(Surv(males, males_censor) ~ 1, conf.type = "log", conf.int = 0.95, times = t1)
ci_males_t2 <- survfit(Surv(males, males_censor) ~ 1, conf.type = "log", conf.int = 0.95, times = t2)

# For females
ci_females_t1 <- survfit(Surv(females, females_censor) ~ 1, conf.type = "log", conf.int = 0.95, times = t1)
ci_females_t2 <- survfit(Surv(females, females_censor) ~ 1, conf.type = "log", conf.int = 0.95, times = t2)

# Print the confidence intervals
cat("95% CI for males at t=1:", round(ci_males_t1$lower, 3), "-", round(ci_males_t1$upper, 3), "\n")
cat("95% CI for males at t=3:", round(ci_males_t2$lower, 3), "-", round(ci_males_t2$upper, 3), "\n")
cat("95% CI for females at t=1:", round(ci_females_t1$lower, 3), "-", round(ci_females_t1$upper, 3), "\n")
cat("95% CI for females at t=3:", round(ci_females_t2$lower, 3), "-", round(ci_females_t2$upper, 3), "\n")

# 3. Median survival times
median_males <- summary(km_males)$table["median"]
median_females <- summary(km_females)$table["median"]

cat("Median survival time for males:", median_males, "\n")
cat("Median survival time for females:", ifelse(is.na(median_females), "Cannot be determined", median_females), "\n")

male_table <- data.frame(
  time_interval = c("0-1", "1-3", "3-10", "10-12", "12-18", "18+"),
  nj = c(6, 6, 5, 3, 2, 1),
  dj = c(0, 1, 1, 1, 1, 0),
  nj_minus_dj_div_nj = c(1, 5/6, 4/5, 2/3, 1/2, 1),
  survival_prob = c(1, 0.833, 0.667, 0.444, 0.222, 0.222)
)

female_table <- data.frame(
  time_interval = c("0-1", "1-3", "3-6", "6-10", "10-11", "11-12"),
  nj = c(6, 6, 5, 4, 4, 4),
  dj = c(0, 0, 1, 0, 0, 1),
  nj_minus_dj_div_nj = c(1, 1, 4/5, 1, 1, 3/4),
  survival_prob = c(1, 1, 0.8, 0.8, 0.8, 0.6)
)

# Print the tables
print(male_table)
print(female_table)
