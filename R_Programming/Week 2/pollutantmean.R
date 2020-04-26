pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return: The mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  files_dir <- list.files(path = directory) # Import all the files from the given directory
  means <- 0
  count <- 0
  
  for (file in id) {
    fp <- paste(directory, files_dir[file], sep="/")
    data <- read.csv(fp)[[pollutant]]
    
    means <- means + mean(data, na.rm=TRUE)
    count <- count + 1
  }
  
  return (means/count)
}

