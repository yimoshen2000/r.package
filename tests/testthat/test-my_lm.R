test_that("returns correct first coefficient", {
  grades_data <- read.csv("https://www.openintro.org/data/csv/gpa.csv")
  myTest <- my_lm(formula = gpa ~ studyweek, data = grades_data)
  my_first_coefficient <- myTest$beta[1]
  correct_first_coefficient <- summary(lm(formula = gpa ~ studyweek, data = grades_data))$coefficients[1, "Estimate"]
  expect_equal(my_first_coefficient, correct_first_coefficient)
})

test_that("returns correct second coefficient", {
  grades_data <- read.csv("https://www.openintro.org/data/csv/gpa.csv")
  myTest <- my_lm(formula = gpa ~ studyweek, data = grades_data)
  my_second_coefficient <- myTest$beta[2]
  correct_second_coefficient <- summary(lm(formula = gpa ~ studyweek, data = grades_data))$coefficients[2, "Estimate"]
  expect_equal(my_second_coefficient, correct_second_coefficient)
})

test_that("returns correct first standard error", {
  grades_data <- read.csv("https://www.openintro.org/data/csv/gpa.csv")
  myTest <- my_lm(formula = gpa ~ studyweek, data = grades_data)
  my_first_se <- myTest$se[1]
  correct_first_se <- summary(lm(formula = gpa ~ studyweek, data = grades_data))$coefficients[1, "Std. Error"]
  expect_equal(my_first_se, correct_first_se)
})

test_that("returns correct second standard error", {
  grades_data <- read.csv("https://www.openintro.org/data/csv/gpa.csv")
  myTest <- my_lm(formula = gpa ~ studyweek, data = grades_data)
  my_second_se <- myTest$se[2]
  correct_second_se <- summary(lm(formula = gpa ~ studyweek, data = grades_data))$coefficients[2, "Std. Error"]
  expect_equal(my_second_se, correct_second_se)
})

