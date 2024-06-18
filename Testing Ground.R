predictors <- c("age_at_encounter", "los_in_hours")

plot_linear_association <- function(data, outcome, predictors) {
  results <- list()
  logit <- function(pr) log(pr/(1-pr))
  for (predictor in predictors) {
    if (is.numeric(data[[predictor]])) {
      loessfit <- predict(loess(data[[outcome]]~data[[predictor]]))
      pi <- pmax(pmin(loessfit,0.9999),0.0001)
      logitfitted <- logit(pi)
      o <- order(data[[predictor]])
      plot(x=data[[predictor]][o],
           y=logitfitted[o],
           type="l",
           xlab=predictor,
           ylab=outcome)
      results$plot[[predictor]] <- recordPlot()
    }
  }
  return(results)
}



results <- plot_linear_association(d_analysis, outcome_variables[1], predictors)
plot_linear_association <- function(data, outcome, predictors) {
  results <- list()
  logit <- function(pr) log(pr/(1-pr))
  for (predictor in predictors) {
    if (is.numeric(data[[predictor]])) {
      # loessfit <- predict(loess(data[[outcome]]~data[[predictor]]))
      # pi <- pmax(pmin(loessfit,0.9999),0.0001)
      # logitfitted <- logit(pi)
      # o <- order(data[[predictor]])
      results$plot[[predictor]] <- ggplot(data=d_analysis,
                                          aes(.data[[predictor]], 
                                              .data[[outcome]])) +
        geom_line() +
        labs(x = predictor, y = outcome)
    }
  }
  return(results)
}

################################
outcome_vars <- c("los_in_hours")
predictor_vars <- c("age_at_encounter", "los_in_hours")

plot_association <- function(data, outcome_var, predictor_var) {
  plot(data[[predictor_var]], data[[outcome_var]],
       xlab = predictor_var, ylab = outcome_var,
       main = paste(outcome_var, "vs", predictor_var))
}

# Use lapply to apply the function to the variable name lists
lapply(outcome_vars, function(outcome_var) {
  lapply(predictor_vars, function(predictor_var) {
    plot_association(d_analysis, outcome_var, predictor_var)
  })
})

library(gridExtra)

plot_grid <- do.call(grid.arrange, c(unlist(readmission_plots, recursive = FALSE), ncol = length(readmission_plots)))
grid.arrange(grobs=list(readmission_plots$age_at_encounter, readmission_plots$los_in_hours))

library(grid)
library(ggplotify)


plot_linear_association <- function(data, outcome, predictors) {
  results <- list()
  logit <- function(pr) log(pr/(1-pr))
  for (predictor in predictors) {
    if (is.numeric(data[[predictor]])) {
      loessfit <- predict(loess(data[[outcome]]~data[[predictor]]))
      pi <- pmax(pmin(loessfit,0.9999),0.0001)
      logitfitted <- logit(pi)
      o <- order(data[[predictor]])
      results$plot[[predictor]] <- as.grob(~plot(x=data[[predictor]][o],
                                                y=logitfitted[o],
                                                type="l",
                                                xlab=predictor,
                                                ylab=outcome))
    }
  }
  return(results)
}


############################
library(ggplot2)

plot_linear_association <- function(data, outcome, predictors) {
  results <- list()
  logit <- function(pr) log(pr/(1-pr))
  for (predictor in predictors) {
    if (is.numeric(data[[predictor]])) {
      loessfit <- predict(loess(data[[outcome]]~data[[predictor]]))
      pi <- pmax(pmin(loessfit,0.9999),0.0001)
      logitfitted <- logit(pi)
      o <- order(data[[predictor]])
      plot_data <- data.frame(x = data[[predictor]][o], y = logitfitted[o])
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

grid.arrange(grobs=readmission_plots, top="30-Day Readmission")
