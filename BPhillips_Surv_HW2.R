# Survival Analysis HW2
# Brendan Phillips

# 1. Kaplan-Meier estimates
library(survival)

# Survival times for males and females
males <- c(1, 3, 4, 10, 12, 18)
females <- c(1, 3, 6, 10, 11, 12)

# Censoring indicators (0 = censored, 1 = event)
males_censor <- c(1, 1, 0, 1, 1, 1)
females_censor <- c(1, 0, 1, 1, 1, 1)

km_males <- survfit(Surv(males, males_censor) ~ 1, conf.type = "log", conf.int = 0.95)
km_females <- survfit(Surv(females, females_censor) ~ 1, conf.type = "log", conf.int = 0.95)

# Tables
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
ci_males <- summary(km_males, times = c(t1,t2))

# For females
ci_females <- summary(km_females, times = c(t1,t2))

# Print the confidence intervals
cat("95% CI for males at t=1: [", round(ci_males$lower[1], 3), ",", round(ci_males$upper[1], 3), "]\n")
cat("95% CI for males at t=3: [", round(ci_males$lower[2], 3), ",", round(ci_males$upper[2], 3), "]\n")
cat("95% CI for females at t=1: [", round(ci_females$lower[1], 3), ",", round(ci_females$upper[1], 3), "]\n")
cat("95% CI for females at t=3: [", round(ci_females$lower[2], 3), ",", round(ci_females$upper[2], 3), "]\n")

# Using Greenwood:

males_se_t1 <- sqrt(ci_males$surv[1]^2*(1/(ci_males$n.risk[1]*(ci_males$n.risk[1]-1))))
males_se_t2 <- sqrt(ci_males$surv[2]^2*(1/(ci_males$n.risk[1]*(ci_males$n.risk[1]-1))+
                                          1/(ci_males$n.risk[2]*(ci_males$n.risk[2]-1))))

ci_males$surv[1] + c(males_se_t1*1.96, males_se_t1*-1.96)
ci_males$surv[2] + c(males_se_t2*1.96, males_se_t2*-1.96)

females_se_t1 <- sqrt(ci_females$surv[1]^2*(1/(ci_females$n.risk[1]*(ci_females$n.risk[1]-1))))

# For females, the estimates and 95% CI are the same at t=1 and t=3 as no deaths
# occur between those time points.
ci_females$surv[1] + c(females_se_t1*1.96, females_se_t1*-1.96)

# All the CI limits that are greater than 1 would round down to 1 given a survival
# percentage greater than 1 is not possible.

# 3. Median survival times
median_males <- summary(km_males)$table["median"]
median_females <- summary(km_females)$table["median"]

cat("Median survival time for males:", median_males, "\n")
cat("Median survival time for females:", median_females)


