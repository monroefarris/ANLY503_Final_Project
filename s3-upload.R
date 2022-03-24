# Load Required Libraries ------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, aws.s3)

#' Stores a file in S3
#'  
#' @param file A file
#' @param suffix The type of data. Used to strip from the filename for cleaning purposes
#' @param directory The inner directory used to store the data in S3
storeDataInS3 <- function(file) {
  print('Storing file in s3')
  print(file)
  put_object(file = file, object = file, bucket = 'datastore.portfolio.sampastoriza.com')
  print('Uploaded file to S3 successfully')
}

# Upload all visualization data to the S3 bucket
allFiles <- list.files('visualization_data', full.names = TRUE, pattern = '*', recursive = TRUE)
print('Uploading visualization data to S3')
lapply(allFiles, FUN = function(f) { storeDataInS3(f) })
