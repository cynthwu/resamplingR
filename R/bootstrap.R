#' Performs the bootstrap procedure to make a confidence interval for the specified statistics of a variable
#' Plots a histogram of the bootstrap values with the specified confidence interval cutoffs
#'
#' @param data a data frame
#' @param nreps the number of bootstraps that are to be generated
#' @param var the variable name for the column in the data set
#' @param stats the statistic used to perform the bootstrap procedure
#' @param lower the lower quantile of the confidence interval
#' @param upper the upper quantile of the confidence interval
#'
#'
#' @return the confidence interval and a histogram of the bootstrap values with the confidence interval cutoffs
#'
#' @import tidyverse
#' @import modelr
#' @import purrr
#'
#' @export
bootstrap <- function(data, nreps, var, stats = mean, lower = 0.025, upper = 0.975){
  boots <- data %>%
    modelr::bootstrap(nreps)

  boots <- boots %>%
    mutate(
      boots_stat = map_dbl(strap, ~stats(data.frame(.x)[[var]]))
    )

  quant <- boots %>%
    pull(boots_stat) %>%
    quantile(c(lower, upper))

  print(quant)

  plot <- ggplot(boots, aes(x = boots_stat)) +
    geom_histogram(aes(y = ..density..),
                   fill = "white",
                   color = "black") +
    geom_density(fill = "cornflowerblue", alpha = 0.5) +
    geom_vline(xintercept = quantile(boots$boots_stat, c(lower)), color = "red", lwd = 2) +
    geom_vline(xintercept = quantile(boots$boots_stat, c(upper)), color = "red", lwd = 2)

  return(plot)
}
