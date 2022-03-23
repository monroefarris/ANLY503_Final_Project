# Load Required Libraries ------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse)

# Load Person Data -------------------------------------------------------------

process.person.data <- function(year) {
  print(paste0('Processing person data for ', year))
  person.df <- read.csv(paste0('./data/', year, '/cleaned-person.csv'))
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

years <- seq(1975, 1999, 1)
century.data.ls <- lapply(years, process.person.data)
century.data.df <- do.call('rbind', century.data.ls)

str(century.data.df)

viz.data.group.df <- century.data.df %>%
  group_by(SeatingPosition, SeverityOfInjury) %>%
  count(name='NumberOfPeople') %>%
  mutate(SeatingPosition = factor(SeatingPosition, levels = c('Driver', 'Passenger', 'Back Seat'))) %>%
  mutate(drilldown = as.factor(paste0(SeatingPosition, ' - ', SeverityOfInjury)))

viz.data.df.ddn <- century.data.df %>%
  mutate(AgeGroup = cut(Age, breaks = c(0, 12, 19, 35, 60, 110), include.lowest = TRUE, labels = c('Child (0-12)', 'Teen (13-19)', 'Young Adult (20-35)', 'Adult (35-60)', 'Senior (60+)'))) %>%
  group_by(SeatingPosition, SeverityOfInjury, AgeGroup, RestraintSystemUsed, EjectionStatus) %>%
  count(name='NumberOfPeople') %>%
  ungroup() %>%
  complete(SeatingPosition, SeverityOfInjury, AgeGroup, RestraintSystemUsed, EjectionStatus, fill = list(NumberOfPeople = 0)) %>%
  mutate(drilldown = as.factor(paste0(SeatingPosition, ' - ', SeverityOfInjury))) %>%
  mutate(SeatingPosition = factor(SeatingPosition, levels = c('Driver', 'Passenger', 'Back Seat'))) %>%
  arrange(SeatingPosition)

write.csv(viz.data.df.ddn, 'vehicle_drilldown_data_20.csv', row.names = FALSE)
write.csv(viz.data.group.df, 'vehicle_series_data_20.csv', row.names = FALSE)
