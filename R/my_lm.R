#' Linear Model function
#' 
#' This function fits a linear model.
#' 
#' @param formula a formula class object
#' @param data input data frame
#' @keywords regression, covariance
#' 
#' @return a table for each coefficient and columns for the Estimate, Std. Error, t-value, and p-value
#' 
#' @import stats
#' @examples 
#' grades_data <- read.csv("https://www.openintro.org/data/csv/gpa.csv")
#' my_lm(formula = gpa ~ studyweek, data = grades_data)
#' 
#' @export

my_lm <- function(formula, data) {
  # get sample size from data
  n <- nrow(data)
  # get x and y matrices from input
  X <- model.matrix(formula, data)
  model_frame <- model.frame(formula, data)
  Y <- model.response(model_frame)
  # solve for coefficients with (X^TX)^-1X^TY
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  # get degrees of freedom
  df <- n - ncol(X)
  # estimate sigma_squared
  sigma_squared = 0
  for (i in 1:nrow(X)) {
    current = (Y[i] - X[i, ] %*% beta)^2 / df
    sigma_squared = sigma_squared + current
  }
  # estimate standard error
  se <- sqrt(diag(sigma_squared[1] * solve(t(X) %*% X)))
  # compute test statistic
  test_stat <- (beta - 0) / se
  # compute a p-value
  p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
  result <- data.frame(beta, se, test_stat, p_val)
  table <- kable(result, format = "simple", col.names = c("Estimate", "Std. Error", "t value", "p value"))
  return (table)
}