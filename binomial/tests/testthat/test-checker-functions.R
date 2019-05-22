context("Private Checker Functions Tests")
library(binomial)

#Tests for check_prob()
test_that("Checking for error if input for check_prob() is not value between 0 and 1", {
  expect_error(check_prob(-.5))
  expect_error(check_prob(1.1))
})

test_that("Checking for error if input for check_prob() is not of length 1", {
  expect_length(check_prob(.2), 1)
  expect_error(check_prob(c(.2, .5)))
})

test_that("Checking for error if output for check_prob() is ndevot a logical value", {
  expect_equal(class(check_prob(.1)), 'logical')
})

#Tests for check_trials()
test_that("Checking for error if output for check_trials() is not a postive value", {
  expect_error(check_trials(-2))
})

test_that("Checking for error if output for check_trials() is not an integer value", {
  expect_error(check_trials(1.2))
})

test_that("Checking for error if output for check_trials() is not a logical value", {
  expect_equal(class(check_trials(5)), 'logical')
})

#Tests for check_success()
test_that("Checking for error if success input vector for check_success() is less than trials and valid", {
  expect_equal(check_success(1:5, 10), TRUE)
})

test_that("Checking for error if output for check_success() is not an integer value", {
  expect_error(check_success(1.2, 5))
})

test_that("Checking for error if output for check_success() is not a logical value", {
  expect_equal(class(check_success(5, 10)), 'logical')
})
