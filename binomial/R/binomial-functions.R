#1.3) Function bin_choose()

#' @title Binomial Coefficient
#' @description represents the number of ways one can choose k objects from n number of objects
#' @param n total number of objects, any integer greater than zero
#' @param k number of objects to be selected, any integer or vector less than n
#' @return binomial coefficient, or the number of ways to choose k objects from n # of objects
#' @export
#' @examples bin_choose(n = 5, k = 2) 
#' @examples bin_choose(5, 0)
#' @examples bin_choose(5, 1:3)

bin_choose <- function(n, k)
  {
  if(k > n)
    {
    return("k cannot be greater than n")
    stop()
    }
  else
    {
    ans <- factorial(n)/(factorial(k)*factorial(n - k))
    return(ans)
  }
}

#1.4) Function bin_probability()

#' @title Binomial Probability
#' @description gives the probability of something occurring over a specified number of trials
#' @param success integer, less than or equal to the # of trials
#' @param trials which must be an integer or vector of integers, greater than zero
#' @param prob a number between 0 and 1
#' @return returns the probability of a # of successes occurring for a given number of trials
#' @export
#' @examples bin_probability(success = 2, trials = 5, prob = 0.5)
#' @examples bin_probability(0:2, 5, 0.5) 

bin_probability <- function(success, trials, prob){
  if(check_success(success, trials) != TRUE || check_trials(trials) != TRUE || check_prob(prob) != TRUE)
    {
    return("value inputted is invalid")
    stop()
    }
  else
    {
    bin_choice <- bin_choose(n = trials, k = success)
    bin_prob <- (prob^(success))*((1 - prob)^(trials - success))
    bin_ans <- bin_prob*bin_choice
    return(bin_ans)
  }
}

#1.5) Function bin_distribution()

#' @title Binomial Distribution
#' @description probability distribution for a given number of trials, output can also be directly plotted using plot()
#' @param trials must be an integer, greater than zero
#' @param prob a number between 0 and 1
#' @return a dataframe with the # of successes, and the associated probability distribution
#' @export
#' @examples bin_distribution(trials = 10, prob = 0.5)
#' @examples bin_distribution(8, 0.4)

bin_distribution <- function(trials, prob)
  {
  check_prob(prob)
  check_trials(trials)
  successes <- c(0:trials)
  prob <- bin_probability(success = 0:trials, trials = trials, prob = prob)
  data_frame1 <- data.frame(successes, prob)
  class(data_frame1) <- c("bindis", "data.frame")
  return(data_frame1)
}

#' @export
plot.bindis <- function(values)
  {
  graph1 <- barplot(values$prob, xlab = "# of success", ylab = "Probability", names.arg = values$success)
  return(graph1)
}

#1.6) Function bin_cumulative()

#' @title Binomial Cumulative
#' @description will give a data frame of both the probabilities and the cumulative sum associated with a given number of successes.Can then be plotted directly using plot() function.
#' @param trials must be an integer, greater than zero
#' @param prob a number between 0 and 1
#' @return data frame with # of successes, probability, and cumulative sum
#' @export
#' @examples bin_cumulative(trials = 5, prob = 0.5)
#' @examples bin_cumulative(10, 0.2)

bin_cumulative <- function(trials, prob)
  {
  check_trials(trials)
  check_prob(prob)
  success <- c(0:trials)
  probability <- bin_probability(success = 0:trials, trials = trials, prob = prob)
  cumulative <- cumsum(probability)
  data_frame2 <- data.frame(success, probability, cumulative)
  class(data_frame2) <- c("bincum", "data.frame")
  return(data_frame2)
}

#' @export
plot.bincum <- function(values)
  {
  graph2 <- plot(values$success, values$cumulative, xlab = "# of success", ylab = "Probability", type = "o")
  return(graph2)
}

#1.7) Function bin_variable()

#' @title Binomial Variable
#' @description function which allows you to format your trials and prob, as well as summarizing using the print() or summary(), which will give you more information on your binomial distribution
#' @param trials which must be an integer or vector of integers, greater than zero
#' @param prob which is a number between 0 and 1
#' @return outputs your number of trials, and the probability of success in a formatted way
#' @export
#' @examples bin_variable(trials = 5, prob = 0.5)
#' @examples bin_variable(10, 0.3)
#' @examples bin_variable(1:10, 0.4)

bin_variable <- function(trials, prob)
  {
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE)
    {
    ans <- list(trials, prob)
    names(ans) <- c("trials", "prob")
    class(ans) <- "binvar"
    return(ans)
    }
  else
    {
    stop("value inputted is invalid")
  }
}

#' @export
print.binvar <- function(values)
  {
  if(class(x) == 'binvar')
    {
    cat('"Binomial Variable"')
    cat("\n\n", append = TRUE)
    cat("Parameters", append = TRUE)
    cat("\n-# of trials:", values$trials, append = TRUE)
    cat("\n-probability of success:", values$prob, append = TRUE)
    }
  else
    {
    stop("Invalid type of object")
    }
  }

#' @export
summary.binvar <- function(values)
  {
  if(class(x) == 'binvar')
    {
    ans_dat <- list(trials <- values$trials,
                     prob <- values$prob,
                     aux_mean(trials, prob),
                     aux_variance(trials, prob),
                     aux_mode(trials, prob),
                     aux_skewness(trials, prob),
                     aux_kurtosis(trials, prob))
    names(ans_dat) <- c("trials", "prob", "mean", "variance", "mode", "skewness", "kurtosis")
    class(ans_dat) <- 'summary.binvar'
    return(ans_dat)
    }
  else
    {
    stop("Invalid type of object")
    }
  }

#' @export
print.summary.binvar <- function(values)
  {
  cat('"Summary Binomial"')
  cat("\n\n", append = TRUE)
  cat("Parameters", append = TRUE)
  cat("\n-# of trials:", append = TRUE)
  cat("\n-probability of success:", append = TRUE)
  cat("\n\nMeasures\n", append = TRUE)
  cat("-mean:", x$mean, "\n-variance:", values$variance, "\n-mode:", values$mode,
      "\n-skewness:", values$skewness, "\n-kurtosis:", values$kurtosis)
}

#1.8) Functions of Measure

#' @export
bin_mean <- function(trials, prob)
  {
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE)
    {
    return(aux_mean(trials, prob))
    }
  else
    {
    stop("Invaid value inputted")
    }
  }

#' @export
bin_mode <- function(trials, prob)
  {
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE)
    {
    return(aux_mode(trials, prob))
    }
  else
    {
    stop("Invaid value inputted")
    }
  }

#' @export
bin_skewness <- function(trials, prob)
  {
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE)
    {
    return(aux_skewness(trials, prob))
    }
  else
    {
    stop("Invaid value inputted")
    }
  }

#' @export
bin_kurtosis <- function(trials, prob)
  {
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE)
    {
    return(aux_kurtosis(trials, prob))
    }
  else
    {
    stop("Invaid value inputted")
    }
  }

#' @export
bin_variance <- function(trials, prob)
  {
  if(check_prob(prob) == TRUE && check_trials(trials) == TRUE)
    {
    return(aux_variance(trials, prob))
    }
  else
    {
    stop("Invaid value inputted")
    }
}
