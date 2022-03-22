# Load Required Libraries ------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, highcharter)

# Load Person Data -------------------------------------------------------------

year <- 1990
person.df <- read.csv(paste0('./data/', year, '/cleaned-person.csv'))

viz.data.df <- person.df %>%
  select(Age, Sex, SeverityOfInjury, SeatingPosition) %>%
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
  mutate(SeatingPosition = as.factor(SeatingPosition),
         SeverityOfInjury = as.factor(SeverityOfInjury))

viz.data.group.df <- viz.data.df %>%
  group_by(SeatingPosition, SeverityOfInjury) %>%
  count(name='NumberOfPeople') %>%
  mutate(SeatingPosition = factor(SeatingPosition, levels = c('Driver', 'Passenger', 'Back Seat'))) %>%
  mutate(drilldown = as.factor(paste0(SeatingPosition, ' - ', SeverityOfInjury)))

viz.data.df.ddn <- viz.data.df %>%
  mutate(AgeGroup = cut(Age, breaks = c(0, 12, 19, 35, 60, 110), include.lowest = TRUE, labels = c('Child (0-12)', 'Teen (13-19)', 'Young Adult (20-35)', 'Adult (35-60)', 'Senior (60+)'))) %>%
  group_by(SeatingPosition, SeverityOfInjury, AgeGroup, Sex) %>%
  count(name='NumberOfPeople') %>%
  mutate(drilldown = as.factor(paste0(SeatingPosition, ' - ', SeverityOfInjury))) %>%
  mutate(SeatingPosition = factor(SeatingPosition, levels = c('Driver', 'Passenger', 'Back Seat'))) %>%
  arrange(SeatingPosition)

print(viz.data.df.ddn)
table(viz.data.df$Age)
table(viz.data.group.df$drilldown)

write.csv(viz.data.df.ddn, 'vehicle_drilldown_data_1990.csv', row.names = FALSE)
write.csv(viz.data.group.df, 'vehicle_series_data_1990.csv', row.names = FALSE)
