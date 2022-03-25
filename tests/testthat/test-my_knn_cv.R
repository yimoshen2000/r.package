test_that("returns correct output", {
  penguins <- na.omit(my_penguins[, 1:6])
  train <- penguins[, 3:6]
  myTest <- my_knn_cv(train, penguins$species, 1, 5)
  expect_type(myTest, "list")
  expect_type(myTest$cv_err, "double")
})


