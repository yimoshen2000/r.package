#' T-test function
#' 
#' This function performs a one sample t-test.
#' 
#' @param x a numeric vector
#' @param alternative a character string specifying the alternative hypothesis
#' @param mu a number indicating the null hypothesis value of the mean
#' @keywords t-test
#' 
#' @return a list of following elements:
#'   \code{test_stat}: numeric test statistic
#'   \code{df}: degrees of freedom
#'   \code{alternative}: value of the parameter \code{alternative}
#'   \code{p_val}: p-value
#' @examples
#' helium_data <- read.csv("https://www.openintro.org/data/csv/helium.csv")
#' my_t.test(helium_data[3], alternative = "greater", mu = 20)
#' @export

my_t.test <- function(x, alternative, mu) {
  # get estimated mean
  x <- unlist(x)
  estimated_mean <- mean(x)
  # compute standard error
  sample_size <- length(x)
  estimated_se <- sd(x) / sqrt(sample_size)
  # compute test statistic
  test_stat <- (estimated_mean - mu) / estimated_se
  # find probability of test statistic
  df <- sample_size - 1
  if (alternative == "less") {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  } else if (alternative == "two.sided"){
    p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
  } else {
    message("*alternative* input must be less, greater, or two.sided!")
    return(alternative)
  }
  # return output list
  output <- list(test_stat, df, alternative, p_val)
  return (output)
}