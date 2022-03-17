#' K-Nearest Neighbor Cross-validation function
#' 
#' This function performs cross-validation in R
#' 
#' @param train input data frame
#' @param cl true class value of your training data
#' @param k_nn integer representing the number of neighbors
#' @param k_cv integer representing the number of folds
#' @keywords classification, prediction
#' 
#' @return list with following 2 elements: 
#'   \code{class}: a vector of the predicted class Yi for all observations
#'   \code{cv_err}: a numeric with the cross-validation mis-classification error
#'    
#' @import knitr kableExtra class dplyr
#' @examples
#' penguins <- na.omit(my_penguins[, 1:6])
#' train <- penguins[, 3:6]
#' my_knn_cv(train, penguins$species, 1, 5)
#' 
#' @export

my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # filter out any observations that have missing values
  set.seed(256)
  # Split data in k_cv parts, randomly
  n = nrow(train)
  fold <- sample(rep(1:k_cv, length = n))
  data <- data.frame("x" = train, "y" = cl, "split" = fold)
  
  missclassification = rep(NA, k_cv)
  for (i in 1:k_cv) {
    data_train <- data %>% dplyr::filter(split != i)
    data_test <- data %>% dplyr::filter(split == i)
    # predict the class of the ith fold using all other folds as the training data
    y_hat <- knn(data_train[,1:4], data_test[,1:4], as.factor(data_train$y), k_nn)
    # record the mis-classification rate
    missclassification[i] <- sum(y_hat != data_test$y)/nrow(data_test)
  }
  # store the vector class as the output of knn() with the full data as both the training and the test data
  class <- knn(train, train, as.factor(data$y), k_nn)
  # Compute average to get CV error
  cv_err <- mean(missclassification)
  return(list("class"= class,"cv_err" = cv_err))
}