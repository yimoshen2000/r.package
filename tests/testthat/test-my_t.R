test_that("alternative greater than", {
  helium_data <- read.csv("https://www.openintro.org/data/csv/helium.csv")
  myTest <- my_t.test(helium_data[3], alternative = "greater", mu = 20)
  correctTest <- t.test(helium_data[3], mu = 20, alternative = "greater")
  
  expect_equal(myTest[[1]], as.numeric(correctTest$statistic))
  expect_equal(myTest[[2]], as.numeric(correctTest$parameter))
  expect_equal(myTest[[4]], as.numeric(correctTest$p.value))
})

test_that("alternative less than", {
  helium_data <- read.csv("https://www.openintro.org/data/csv/helium.csv")
  myTest <- my_t.test(helium_data[3], alternative = "less", mu = 20)
  correctTest <- t.test(helium_data[3], mu = 20, alternative = "less")
  
  expect_equal(myTest[[1]], as.numeric(correctTest$statistic))
  expect_equal(myTest[[2]], as.numeric(correctTest$parameter))
  expect_equal(myTest[[4]], as.numeric(correctTest$p.value))
})

test_that("alternative two-sided", {
  helium_data <- read.csv("https://www.openintro.org/data/csv/helium.csv")
  myTest <- my_t.test(helium_data[3], alternative = "two.sided", mu = 20)
  correctTest <- t.test(helium_data[3], mu = 20, alternative = "two.sided")
  
  expect_equal(myTest[[1]], as.numeric(correctTest$statistic))
  expect_equal(myTest[[2]], as.numeric(correctTest$parameter))
  expect_equal(myTest[[4]], as.numeric(correctTest$p.value))
})

test_that("error alternative", {
  helium_data <- read.csv("https://www.openintro.org/data/csv/helium.csv")
  expect_error(my_t.test(lifeExp, "two", 20))
})

