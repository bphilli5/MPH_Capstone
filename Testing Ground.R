plot_linear_association <- function(data, outcome, predictors) {
  results <- list()
  logit <- function(pr) log(pr/(1-pr))
  for (predictor in predictors) {
    if (is.numeric(data[[predictor]])) {
      data <- data[complete.cases(data[c(outcome,predictor)]),]
      loessfit <- predict(loess(data[[outcome]]~data[[predictor]]), se=T)
      pi <- pmax(pmin(loessfit,0.9999),0.0001)
      logitfitted <- logit(pi)
      o <- order(data[[predictor]])
      plot_data <- data.frame(x = data[[predictor]][o],
                              y = logitfitted[o])
      plot <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_line() +
        labs(x = predictor, y="")
      results[[predictor]] <- plot
    }
  }
  return(results)
}

data <- d_analysis
outcome <- c("readmission_30days_recode_analysis")
predictor <- c("age_at_encounter")

tester <- plot_linear_association(data,outcome,predictor)

ggplot(data=d_analysis, aes(.data[[predictor]], .data[[outcome]])) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = T)
