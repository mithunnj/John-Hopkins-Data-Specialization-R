corr <- function(directory, threshold = 0) {
  ## 'directory' is a characte vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the 
  ## number of completly observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # Get a data frame of all files that satisfy the threshold
  
  files <- complete(directory, t_check = TRUE, threshold = threshold)[["id"]]
  
  # Read information from each valid file, and store the information in a data frame
  sulfate_nitrate_data <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(sulfate_nitrate_data) <- c("sulfate", "nitrate") # Column name titles set for the dataframe
  files_dir <- list.files(path = directory) # Import all the files from the given directory
  
  for (file in files) {
    fp <- paste(directory, files_dir[file], sep="/") # Build filepath for the .csv file
    file_content <- read.csv(fp) # Read the csv data 
    
    complete_data <- file_content[complete.cases(file_content),] # Return all rows with both sulfate and nitrate defined
    
    sulfate_nitrate_data <- rbind(sulfate_nitrate_data, data.frame(sulfate = complete_data[["sulfate"]], nitrate = complete_data[["nitrate"]]))
  }
  
  cor(sulfate_nitrate_data$sulfate, sulfate_nitrate_data$nitrate)
  
}