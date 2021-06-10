#' Performs a permutation test and returns a two-tail p-value
#' Plots a histogram of the differences in the specified sample statistic
#'
#' @param data a data frame
#' @param cat_var the categorical variable of interest
#' @param quant_var the quantitative variable of interest
#' @param stats the statistic used to calculate the data
#' @param nreps the number of permutations
#'
#'
#'
#' @return a two-tail p-value and a histrogram of the differences in the specified sample statistic
#'
#' @import tidyverse
#'
#' @export

permutation_test <- function(data, cat_var, quant_var, nreps, stats = mean) {
  original <- data %>%
    group_by({{cat_var}}) %>%
    summarize(value = stats({{quant_var}})) %>%
    pivot_wider(names_from = {{cat_var}}, values_from = value)

  observed <- original[1,2] - original[1,1]

  results = c()

  vec_quant <- data %>%
    pull({{quant_var}})

  vec_cat <- data %>%
    pull({{cat_var}})

  for (i in 1:nreps){
    shuffled_indices <- sample(1:length(vec_quant), size = length(vec_quant), replace = FALSE)
    shuffled_quant <- vec_quant[shuffled_indices]
    shuffled_df <- data.frame(vec_cat, shuffled_quant)
    original <- shuffled_df %>%
      group_by(vec_cat) %>%
      summarize(value = stats(shuffled_quant)) %>%
      pivot_wider(names_from = vec_cat, values_from = value)
    results[i] <- original[1,2] - original[1,1]
  }
  results <- as.numeric(unlist(results))

  two_tail <- sum(abs(results) >= abs(observed))/(nreps)

  cat(paste("Permutation test p-value:", two_tail))

  df <- data.frame(diff_means = results)

  df %>%
    ggplot(aes(x = diff_means)) +
    geom_histogram(bins = 30) +
    geom_vline(xintercept = unlist(observed), color="deeppink", lwd = 1, linetype = "dashed") +
    xlab("Difference") +
    theme_classic() +
    ggtitle(paste("Differences From", nreps, "Permutations")) +
    theme(plot.title = element_text(hjust = 0.5))
}
