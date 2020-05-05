rankhospital <- function(state, outcome, num = "best") {
  ##################################################
  ## param: state <chr> - Two letter abbreviation of the state
  ## param: outcome <chr> - Health outcome (ex. heart attack, heart failure, pneumonia)
  ## param: num <int/chr> - Rank of the hospital to return
  ## return: 
  ##################################################
  
  rank <- NULL
  data <- best(state, outcome, TRUE) # Retrieve hospital data for the given state and outcome
  
  ## Determine rank
  if (num == "best") {
    rank <- 1
  } else if (num == "worst") {
    rank <- nrow(data)
  } else {
    if ( (num > 0)&(num < nrow(data)) ) {
      rank <- num
    } else {
      return(NA)
    } 
  }
  
  ## Given a rank, get the mortality rate
  mortality_val <- data[rank,2]

  ## Get a list of hospitals with that mortality rate
  titles <- names(data)
  hospital_list <- subset(data, data[[titles[2]]] == mortality_val, select= Hospital.Name)
  
  if (length(hospital_list) > 1) {
    return(sort(hospital_list))
  } else {
    return(hospital_list)
  }
}