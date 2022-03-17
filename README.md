
# r.package

<!-- badges: start -->
[![R-CMD-check](https://github.com/yimoshen2000/r.package/workflows/R-CMD-check/badge.svg)](https://github.com/yimoshen2000/r.package/actions)
<!-- badges: end -->

The goal of r.package is to gain experience on my first well-documented, well-tested, and well-explained R package. This R package include the following functions I’ve written:

- my_t.test
- my_lm
- my_knn_cv

## Installation

You can install the development version of r.package from [GitHub](https://github.com/) with:


``` r
# install.packages("devtools")
devtools::install_github("yimoshen2000/r.package")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(r.package)
# my_t.test
helium_data <- read.csv("https://www.openintro.org/data/csv/helium.csv")
my_t.test(helium_data[3], alternative = "greater", mu = 20)
# my_lm
grades_data <- read.csv("https://www.openintro.org/data/csv/gpa.csv")
my_lm(formula = gpa ~ studyweek, data = grades_data)
# my_knn_cv
penguins <- na.omit(my_penguins[, 1:6])
train <- penguins[, 3:6]
my_knn_cv(train, penguins$species, 1, 5)
```

