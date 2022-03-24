# Load Required Libraries ------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse)

# Load Person Data -------------------------------------------------------------

#' Creates a directory if it does not exist yet
#' 
#' @param path A path to create
#' @examples 
#' createDirectoryIfNotExists('path/to/file')
createDirectoryIfNotExists <- function(path) {
  # Create the directory if it doesn't exist.
  if (dir.exists(path) == FALSE) {
    dir.create(path, recursive = TRUE)
  }
}

process.person.data <- function(year) {
  person.df <- read.csv(paste0('./data/', year, '/cleaned-person.csv'))
  print(paste0('Processing person data for ', year))
  viz.data.df <- person.df %>%
    select(Age, Sex, SeverityOfInjury, SeatingPosition, RestraintSystemUsed, EjectionStatus) %>%
    filter(Sex != '') %>%
    filter(Age != '') %>%
    mutate(SeatingPosition = ifelse(SeatingPosition == 'Front Left', 'Driver', SeatingPosition)) %>%
    mutate(SeatingPosition = ifelse(SeatingPosition %in% c('Front Middle','Front Right', 'Front Other'), 'Passenger', SeatingPosition)) %>%
    mutate(SeatingPosition = ifelse(SeatingPosition %in% c('Second Left', 'Second Middle', 'Second Right', 'Second Other', 
                                                 'Second Other', 'Third Left', 'Third Middle', 'Third Right', 
                                                 'Third Other', 'Third Other','Fourth Left', 'Fourth Middle', 
                                                 'Fourth Right', 'Fourth Other', 'Fourth Other', 'Cab Sleeper', 'Other'), 'Back Seat', SeatingPosition)) %>%
    filter(SeatingPosition %in% c('Driver', 'Passenger', 'Back Seat')) %>%
    mutate(SeverityOfInjury = ifelse(SeverityOfInjury %in% c('Uninjured'), 'Uninjured', SeverityOfInjury)) %>%
    mutate(SeverityOfInjury = ifelse(SeverityOfInjury %in% c('Possible Injury', 'Minor Injury', 'Serious Injury', 'Unknown Injury'), 'Injured', SeverityOfInjury)) %>%
    mutate(SeverityOfInjury = ifelse(SeverityOfInjury %in% c('Fatal Injury', 'Died Prior to Crash'), 'Fatal Injury', SeverityOfInjury)) %>%
    filter(SeverityOfInjury %in% c('Uninjured', 'Injured', 'Fatal Injury')) %>%
    mutate(RestraintSystemUsed = ifelse(RestraintSystemUsed %in% c('None Used'), 'None Used', RestraintSystemUsed)) %>%
    mutate(RestraintSystemUsed = ifelse(RestraintSystemUsed %in% c('Shoulder Belt', 'Lap Belt', 'Restraint Used - Improper', 'Restraint Used, Other'), 'Partial Restraint Used', RestraintSystemUsed)) %>%
    mutate(RestraintSystemUsed = ifelse(RestraintSystemUsed %in% c('Lap and Shoulder Belt', 'Child Safety Seat', 'Helmet', 
                                                                   'Racing Style Harness Used', 'Child Restraint System',
                                                                   'Booster Seat'), 'Proper Restraint Used', RestraintSystemUsed)) %>%
    filter(RestraintSystemUsed %in% c('None Used', 'Partial Restraint Used', 'Proper Restraint Used')) %>%
    mutate(EjectionStatus = ifelse(EjectionStatus %in% c('Partially Ejected', 'Ejected - Unknown Degree', 'Totally Ejected'), 'Ejected', EjectionStatus)) %>%
    filter(EjectionStatus %in% c('Not Ejected', 'Ejected')) %>%
    mutate(SeatingPosition = as.factor(SeatingPosition),
           SeverityOfInjury = as.factor(SeverityOfInjury),
           RestraintSystemUsed = as.factor(RestraintSystemUsed),
           EjectionStatus = as.factor(EjectionStatus))
  
  return(viz.data.df)
}

process.group.df <- function(data.df) {
  viz.data.group.df <- data.df %>%
    group_by(SeatingPosition, SeverityOfInjury) %>%
    count(name='NumberOfPeople') %>%
    mutate(SeatingPosition = factor(SeatingPosition, levels = c('Driver', 'Passenger', 'Back Seat'))) %>%
    mutate(drilldown = as.factor(paste0(SeatingPosition, ' - ', SeverityOfInjury)))
  return(viz.data.group.df)
}

process.drilldown.df <- function(data.df) {
  viz.data.df.ddn <- data.df %>%
    mutate(AgeGroup = cut(Age, breaks = c(0, 12, 19, 35, 60, 110), include.lowest = TRUE, labels = c('Child (0-12)', 'Teen (13-19)', 'Young Adult (20-35)', 'Adult (35-60)', 'Senior (60+)'))) %>%
    group_by(SeatingPosition, SeverityOfInjury, AgeGroup, RestraintSystemUsed, EjectionStatus) %>%
    count(name='NumberOfPeople') %>%
    ungroup() %>%
    complete(SeatingPosition, SeverityOfInjury, AgeGroup, RestraintSystemUsed, EjectionStatus, fill = list(NumberOfPeople = 0)) %>%
    mutate(drilldown = as.factor(paste0(SeatingPosition, ' - ', SeverityOfInjury))) %>%
    mutate(SeatingPosition = factor(SeatingPosition, levels = c('Driver', 'Passenger', 'Back Seat'))) %>%
    arrange(SeatingPosition)
  return(viz.data.df.ddn)
}

# Process each year individually
years <- seq(1975, 2019, 1)
for (year in years) {
  createDirectoryIfNotExists(paste0('visualization_data/', year))
  
  viz.data.year.df <- process.person.data(year)
  viz.data.group.df <- process.group.df(viz.data.year.df)
  write.csv(viz.data.group.df, paste('visualization_data', year, 'grouped_person_series_data.csv', sep = '/'), row.names = FALSE)
  
  viz.data.df.ddn <- process.drilldown.df(viz.data.year.df)
  write.csv(viz.data.df.ddn, paste('visualization_data', year, 'person_drilldown_series_data.csv', sep = '/'), row.names = FALSE)
}

# Process/combine 20th century data
twenty.century.years = seq(1975, 1999, 1)
print('Processing and combining data for the 20th century')
century.data.ls <- lapply(twenty.century.years, process.person.data)
century.data.df <- do.call('rbind', century.data.ls)

viz.data.group.century.df <- process.group.df(century.data.df)
write.csv(viz.data.group.century.df, paste('visualization_data', 'grouped_person_series_data_20_century.csv', sep = '/'), row.names = FALSE)

viz.data.df.century.ddn <- process.drilldown.df(century.data.df)
write.csv(viz.data.df.century.ddn, paste('visualization_data', 'person_drilldown_series_data_20_century.csv', sep = '/'), row.names = FALSE)

# Process/combine 21st century data
twenty.one.century.years = seq(2000, 2019, 1)
print('Processing and combining data for the 21st century')
century.data.ls <- lapply(twenty.one.century.years, process.person.data)
century.data.df <- do.call('rbind', century.data.ls)

viz.data.group.century.df <- process.group.df(century.data.df)
write.csv(viz.data.group.century.df, paste('visualization_data', 'grouped_person_series_data_21_century.csv', sep = '/'), row.names = FALSE)

viz.data.df.century.ddn <- process.drilldown.df(century.data.df)
write.csv(viz.data.df.century.ddn, paste('visualization_data', 'person_drilldown_series_data_21_century.csv', sep = '/'), row.names = FALSE)
