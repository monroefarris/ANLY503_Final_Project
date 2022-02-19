# ------------------Libraries------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, gridExtra) 
# ---------------------------------------------

# ---------------------------------------------
# Retrieve all files from NHTSA
# ---------------------------------------------

years.of.interest <- seq(from = 1975, to = 2019, by = 1)
years.of.interest

for (year in years.of.interest) {
  working.directory <- paste0('./data/', year)
  
  print(paste0('Downloading and extracting accident data from the year ', year))
  temp <- tempfile()
  download.file(paste0('https://www.nhtsa.gov/file-downloads/download?p=nhtsa/downloads/FARS/', year, '/National/FARS', year, 'NationalCSV.zip'), temp)
  extracted.file.names <- grep('person.csv|vehicle.csv|accident.csv$', unzip(temp, list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
  unzip(temp, exdir = working.directory, files = extracted.file.names)
  file.names <- list.files(working.directory)
  new.file.names <- c('accident.csv', 'person.csv', 'vehicle.csv')
  file.rename(paste0(working.directory, '/', file.names),
              paste0(working.directory, '/', new.file.names))
  unlink(temp)
}

# ---------------------------------------------
# Combine and clean data per year
# ---------------------------------------------

total.rows <- 0
for (year in years.of.interest) {
  accident.df <- read.csv(paste0('./data/', year, '/accident.csv'))
  total.rows <- total.rows + nrow(accident.df)
}
print(total.rows)
