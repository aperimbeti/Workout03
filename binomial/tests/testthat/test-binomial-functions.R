context("Binomial Functions Tests")
library(binomial)

#Tests for bin_choose()
test_that("Input for bin_choose() is allowed to be a vector", {
  expect_equal(bin_choose(5, 1:3), c(5, 10, 10))
  expect_equal(bin_choose(10, 1:5), c(10, 45, 120, 210, 252))
})

test_that("Error is thrown when k is greater than n", {
  expect_equal(bin_choose(5, 6), "k cannot be greater than n")
  expect_warning(bin_choose(5, 1:10))
})

test_that("Output of bin_choose() is correct combination value", {
  expect_equal(bin_choose(5, 2), 10)
  expect_equal(bin_choose(20, 4), 4845)
})

#Test for bin_probability()
test_that("Input for bin_probability() is allowed to be a vector for success", {
  expect_equal(bin_probability(0:3, 5, 0.3), c(0.16807, 0.36015, 0.30870, 0.13230))
})

test_that("Error is thrown when k is greater than n", {
  expect_error(bin_probability(6, 5, 0.3))
})

test_that("Output vector from bin_probability() is correct length", {
  expect_length(bin_probability(0:5, 10, .3), 6)
  expect_length(bin_probability(5, 10, 0.3), 1)
})

#Tests for bin_distribution()
test_that("Output for bin_distribution() is the correcrt class", {
  expect_equal(class(bin_distribution(5, 0.3)), c('bindis', 'data.frame'))
})

test_that("Error is thrown in bin_distribution() for an invalid probability value", {
  expect_error(bin_distribution(5, 1.2))
})

test_that("Output of bin_distribution() holds correct # of rows", {
  expect_equal(nrow(bin_distribution(5, .3)), 6)
})

#Tests for bin_cumulative()
test_that("End is at 1 and error is thrown otherwise", {
  expect_equal(bin_cumulative(5, 0.3)[6, 3], 1)
})

test_that("Output of bin_cumulative() holds correct # of rows", {
  expect_equal(nrow(bin_cumulative(5, 0.3)), 6)
})

test_that("bin_cumulative gives correct number of columns", {
  expect_equal(ncol(bin_cumulative(5, .3)), 3)
})
