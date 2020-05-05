best <- function(state, outcome, return_list = FALSE) {
  ##################################################
  ## param: state <chr> - Two letter abbreviation of the state
  ## param: outcome <chr> - Health outcome (ex. heart attack, heart failure, pneumonia)
  ## param: return_list <bool> - If TRUE, it will skip the logic of identifying the best hospital, and instead
  # will return the list of hospitals that were generated for the specified state and outcome.
  ## return: dataframe/character vector depending on the bool return_list
  ##################################################
  ## Read outcome data
  data <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
  val_state <- names(table(data$State))
  val_outcome <- c("heart attack", "heart failure", "pneumonia")
  #data_mapping <- data.frame("issue"=c("heart attack", "heart failure", "pneumonia"), "colNum"=c(11,27,23))
  
  ## Validate user inputs
  if (!(state %in% val_state)) stop("invalid state")
  if (!(outcome %in% val_outcome)) stop("invalid outcome")
  
  ## Function to capitalize words in string
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=".")
  }
  
  ## Find list of hospitals with lowest outcome
  mortality_title <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", paste(sapply(outcome, simpleCap), sep = ""), sep = "")
  mortality_index <- match(mortality_title, names(data))
  
  hospital_outcome <- subset(data, State == state, select=c(2, mortality_index)) # Subset the data with the hospital names and the mortality for outcome
  hospital_outcome[,2] <- sapply(hospital_outcome[,2], as.numeric) # Convert the mortality rate from chr to numeric
  hospital_outcome <- hospital_outcome[complete.cases(hospital_outcome),] # Remove all hospitals with NA
  hospital_outcome <- hospital_outcome[order(hospital_outcome[2]),] # Order the data from lowest to heighest death rates
  
  ## Return hospital name in that state with lowest 30-day death rate
  lowest_val <- NULL # Place holder for the lowest mortality rate
  hospital_return <- NULL # Place hold
  
  if (return_list) {
    return(hospital_outcome)
  } else {
    # Loop through the sorted dataframe of hospitals and mortality rate
    for(i in 1:nrow(hospital_outcome)) {
      row <- hospital_outcome[i,] # Retrieve the hospital name and mortality rate columns given a row
      
      # row[1] - contains the names of the hospitals
      # row [2] - contains the mortality rate of the specified outcome for that hospital
      
      if (is.null(lowest_val)) { # If the lowest mortality rate hasn't been determined, store that information and the corresponding hospital name
        lowest_val <- row[2]
        hospital_return <- c(row[1])
      } else { # If the lowest mortality rate has already been determined, check if there are other hospitals with the same mortality rate. 
        if (lowest_val == row[2]) {
          hospital_return <- c(hospital_return, row[1])
        } else { # If there are no other hospitals to add, check that the list is non-atomic, and sort if there is more than one hospital in the list.
          if (length(hospital_return) > 1) {
            return(sort(hospital_return))
          } else {
            return(hospital_return)
          }
        }
      }
    }
  }  

}