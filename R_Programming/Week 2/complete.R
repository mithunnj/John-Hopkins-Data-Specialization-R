complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame 

  files_dir <- list.files(path = directory) # Import all the files from the given directory
  obs_dataframe <- data.frame(matrix(ncol=2, nrow=0)) # Dataframe to store number of observed cases
  colnames(obs_dataframe) <- c("id", "nobs") # Column name titles set for the dataframe
  
  for (file in id) {
    fp <- paste(directory, files_dir[file], sep="/") # Build filepath for the .csv file
    data <- read.csv(fp) # Read the csv data 

    complete_data <- data[complete.cases(data),] # Return all rows with both sulfate and nitrate defined
    
    obs_dataframe <- rbind(obs_dataframe, data.frame(id = file, nobs = nrow(complete_data))) # Store the file id and total data information
  }
  
  return(obs_dataframe)
}