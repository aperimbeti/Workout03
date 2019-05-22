#1.1) Private Checker Functions

check_prob <- function(prob)
  {
  if(prob <= 1 && prob >= 0 && length(prob) == 1){
    return(TRUE)
  }else{
    stop("Invalid p value, value has to be a number between 0 and 1")
  }
}

check_trials <- function(trials)
  {
  if(trials >= 0 && trials %% 1 == 0){
    return(TRUE)
  }else{
    stop("Invalid trials value, must be a postive integer value")
  }
}

check_success <- function(success, trials)
  {
  if(max(success) <= trials && is.vector(success) && max(success) >= 0 && success %% 1 == 0){
    return(TRUE)
  }else{
    stop("invalid success value or # of success cannot exceed # of trials")
  }
}

