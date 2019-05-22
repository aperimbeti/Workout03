context("Private Auxiliary Functions Tests")
library(binomial)

#Tests for aux_mean()
test_that("Output of aux_mean() is the mean", {
  expect_equal(aux_mean(5, 0.5), 2.5)
  expect_equal(aux_mean(10, 0.2), 2)
})

test_that("Output of aux_mean() has length of 1", {
  expect_length(aux_mean(5, 0.5), 1)
  expect_length(aux_mean(10, 0.2), 1)
})

#Tests for aux_mode()
test_that("Output of aux_mode() is the mode", {
  expect_equal(aux_mode(10, 0.3), 3)
  expect_equal(aux_mode(5, 0.5), c(3, 2))
})

#Tests for aux_variance()
test_that("Output of aux_variance() is the variance", {
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_equal(aux_variance(15, 0.5), 3.75)
  expect_equal(aux_variance(10, 0.5), 2.5)
})

#Tests for aux_skewness()
test_that("Output of aux_skewness() is the skew value", {
  expect_equal(aux_skewness(10, 0.4), 0.1290994)
  expect_equal(aux_skewness(10, 0.5), 0)
  expect_equal(aux_skewness(15, 0.2), 0.3872983)
})

#Tests for aux_kurtosis()
test_that("Output of aux_kurtosis() is the kurtosis value ", {
  expect_equal(aux_kurtosis(15, 0.3), -0.0825397)
  expect_equal(aux_kurtosis(10, 0.4), -0.1833333)
  expect_equal(aux_kurtosis(10, 0.8), 0.025)
})
