#1.2) Private Auxilliary Functions

aux_mean <- function(trials, prob)
  {
  avg <- trials*prob
  avg <- round(avg, 7)
  return(avg)
}

aux_variance <- function(trials, prob)
  {
  vari <- trials*prob*(1 - prob)
  vari <- round(vari, 7)
  return(vari)
}

aux_mode <- function(trials, prob)
  {
  if(((trials*prob)+prob) %% 1 == 0)
    {
    mod <- c((trials*prob + prob), (trials*prob + prob - 1))
    return(mod)
    }
  else
    {
    mod <- trials*prob
    return(mod)
  }
}

aux_skewness <- function(trials, prob)
  {
  skewness <- (1 - 2*prob)/(sqrt(prob*trials*(1 - prob)))
  skewness <- round(skewness, 7)
  return(skewness)
}

aux_kurtosis <- function(trials, prob)
  {
  kurt <- (1 - 6*prob*(1 - prob))/(prob*trials*(1 - prob))
  kurt <- round(kurt, 7)
  return(kurt)
}
