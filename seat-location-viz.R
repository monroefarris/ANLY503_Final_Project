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
  count(name='NumberOfPeople')

print(viz.data.df.ddn)
table(viz.data.df$Age)

# viz.data.df %>%
#   ggplot(aes(fill = SeverityOfInjury, x = SeatingPosition, y=NumberOfPeople)) + 
#   geom_bar(position = "dodge", stat='identity') +
#   labs(
#     title = paste0("Locations of Passengers vs Severity of Injury (", year, ")"),
#     fill = "Severity of Injury"
#   ) +
#   xlab("Seat Location") +
#   ylab("Number of People")

table(viz.data.group.df$drilldown)


highchart() %>%
  hc_title(text = paste0("Locations of Passengers vs Severity of Injury (", year, ")")) %>%
  hc_xAxis(type = "category") %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(enabled = TRUE)
    )
  ) %>%
  hc_add_series(
    data = viz.data.group.df,
    type = "column",
    hcaes(group = SeverityOfInjury, color = SeverityOfInjury, x = SeatingPosition, y = NumberOfPeople, drilldown = drilldown)
  ) %>%
  hc_colors(c("red", "#EFC000FF", 'darkgreen')) %>%
  hc_xAxis(
    categories = list(
      'Driver', 
      'Passenger', 
      'Back Seat'
    )
  ) %>% 
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list(
      list(
        id = 'Driver - Fatal Injury',
        type = "column",
        data = list_parse(viz.data.df.ddn %>% 
            filter(SeverityOfInjury == 'Fatal Injury' & SeatingPosition == 'Driver') %>%
            ungroup() %>%
            mutate(group = AgeGroup,
                   color = AgeGroup,
                   x = Sex,
                   y = NumberOfPeople) %>%
            select(group, color, x, y))
      ),
      list(
        id = 'Driver - Injured',
        type = "column",
        data = list_parse(viz.data.df.ddn %>% 
            filter(SeverityOfInjury == 'Injured' & SeatingPosition == 'Driver') %>%
            ungroup() %>%
            mutate(group = AgeGroup,
                   color = AgeGroup,
                   x = Sex,
                   y = NumberOfPeople) %>%
            select(group, color, x, y))
      ),
      list(
        id = 'Driver - Uninjured',
        type = "column",
        data = list_parse(viz.data.df.ddn %>% 
            filter(SeverityOfInjury == 'Uninjured' & SeatingPosition == 'Driver') %>%
            ungroup() %>%
            mutate(group = AgeGroup,
                   color = AgeGroup,
                   x = Sex,
                   y = NumberOfPeople) %>%
            select(group, color, x, y))
      ),
      list(
        id = 'Passenger - Fatal Injury',
        type = "column",
        data = list_parse(viz.data.df.ddn %>% 
            filter(SeverityOfInjury == 'Fatal Injury' & SeatingPosition == 'Passenger') %>%
            ungroup() %>%
            mutate(group = AgeGroup,
                   color = AgeGroup,
                   x = Sex,
                   y = NumberOfPeople) %>%
            select(group, color, x, y))
      ),
      list(
        id = 'Passenger - Injured',
        type = "column",
        data = list_parse(viz.data.df.ddn %>% 
            filter(SeverityOfInjury == 'Injured' & SeatingPosition == 'Passenger') %>%
            ungroup() %>%
            mutate(group = AgeGroup,
                   color = AgeGroup,
                   x = Sex,
                   y = NumberOfPeople) %>%
            select(group, color, x, y))
      ),
      list(
        id = 'Passenger - Uninjured',
        type = "column",
        data = list_parse(viz.data.df.ddn %>% 
            filter(SeverityOfInjury == 'Uninjured' & SeatingPosition == 'Passenger') %>%
            ungroup() %>%
            mutate(group = AgeGroup,
                   color = AgeGroup,
                   x = Sex,
                   y = NumberOfPeople) %>%
            select(group, color, x, y))
      ),
      list(
        id = 'Back Seat - Fatal Injury',
        type = "column",
        data = list_parse(viz.data.df.ddn %>% 
            filter(SeverityOfInjury == 'Fatal Injury' & SeatingPosition == 'Back Seat') %>%
            ungroup() %>%
            mutate(group = AgeGroup,
                   color = AgeGroup,
                   x = Sex,
                   y = NumberOfPeople) %>%
            select(group, color, x, y))
      ),
      list(
        id = 'Back Seat - Injured',
        type = "column",
        data = list_parse(viz.data.df.ddn %>% 
            filter(SeverityOfInjury == 'Injured' & SeatingPosition == 'Back Seat') %>%
            ungroup() %>%
            mutate(group = AgeGroup,
                   color = AgeGroup,
                   x = Sex,
                   y = NumberOfPeople) %>%
            select(group, color, x, y))
      ),
      list(
        id = 'Back Seat - Uninjured',
        type = "column",
        data = list_parse(viz.data.df.ddn %>% 
            filter(SeverityOfInjury == 'Uninjured' & SeatingPosition == 'Back Seat') %>%
            ungroup() %>%
            mutate(group = AgeGroup,
                   color = AgeGroup,
                   x = Sex,
                   y = NumberOfPeople) %>%
            select(group, color, x, y))
      ),
    )
  ) 
  
