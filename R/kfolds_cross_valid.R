#' Performs the K-folds cross validation procedure and returns the mean square errors
#'
#' @param K the number of folds
#' @param data a data frame
#' @param y the response variable for the linear regression
#' @param x a vector of the explanatory variable(s) for the linear regression
#'
#'
#' @return a vector of average mean square errors
#'
#' @import tidyverse
#'
#' @export
kfolds_cross_valid <- function(K, data, y, x) {
  set.seed(123456)
  data <- data %>%
    mutate(
      fold = sample(K, nrow(.), replace = TRUE))
  mse <- c()
  for (i in 1:K) {
    index <- which(data$fold != i)
    train <- data[index, ]
    test <- data[-index, ]
    exp_var <- paste(x, collapse = '+')
    equation <- as.formula(paste(y, exp_var, sep='~'))
    model <- lm(equation, data = train)
    predictions <- predict(model, newdata = test)
    mse[i] <- mean((test[[y]] - predictions)^2)
  }
  return(mse)
}
