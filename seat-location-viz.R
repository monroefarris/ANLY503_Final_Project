# Load Required Libraries ------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, highcharter)

# Load Person Data -------------------------------------------------------------

year <- 1980
person.df <- read.csv(paste0('./data/', year, '/cleaned-person.csv'))

viz.data.df <- person.df %>%
  select(Age, Sex, SeverityOfInjury, SeatingPosition) %>%
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

viz.data.df %>%
  mutate(SeatingPosition = factor(SeatingPosition, levels = c('Driver', 'Passenger', 'Back Seat'))) %>%
  ggplot(aes(fill = SeverityOfInjury, x = SeatingPosition)) + 
  geom_bar(position = "dodge") +
  labs(
    title = paste0("Locations of Passengers vs Severity of Injury (", year, ")"),
    fill = "Severity of Injury"
  ) +
  xlab("Seat Location") +
  ylab("Number of People")
  
