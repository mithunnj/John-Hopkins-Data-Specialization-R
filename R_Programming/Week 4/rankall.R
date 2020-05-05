rankall <- function(outcome, num = "best") {
  ##################################################
  ## param: outcome <chr> - Health outcome (ex. heart attack, heart failure, pneumonia)
  ## param: num <int/chr> - Rank of the hospital to return
  ## return: dataframe containing the best hospital per state
  ##################################################
  hospital_info <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(hospital_info$State) # Get a list of states
  
  # Initialize dataframe to store final result
  hospital_df <- data.frame(matrix(ncol=2, nrow=0))
  colnames(hospital_df) <- c("hospital", "state")
  
  # Loop through each state and generate information for the best hospital for the outcome
  for (state in states) {
    data <- rankhospital(state, outcome, num)
    
    # Add all the information to the dataframe result
    for (info in data) {
      hospital_df[nrow(hospital_df) + 1, ] = c(info, state)
    }
  }
  
  return(hospital_df)
}