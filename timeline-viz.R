# Load Required Libraries ------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, lubridate, leaflet, tigris, leafpop, ggthemes)

# Load Accident Data -----------------------------------------------------------

accident.df <- read.csv('./data/2019/cleaned-accident.csv')
weather.df <- accident.df %>%
  group_by(StateAbbv, WeatherCondition) %>%
  count(name = 'NumberOfCarCrashes') %>%
  filter(WeatherCondition != '') %>%
  filter(WeatherCondition != 'Clear') %>%
  mutate(WeatherCondition = as.factor(WeatherCondition))

weather.df

states <- states(cb=T) %>% filter_state(state.name)
states

merged.df <- geo_join(states, weather.df, 'STUSPS', 'StateAbbv')
merged.df

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

weather.bar.plot <- function(abbv) {
  filtered.data <- weather.df %>% filter(StateAbbv == abbv)
  weather.plot <- filtered.data %>%
    ggplot(aes(x = WeatherCondition, y = NumberOfCarCrashes, fill = WeatherCondition)) +
    geom_bar(stat = 'identity') +
    scale_y_continuous(breaks = integer_breaks()) +
    scale_color_tableau() +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = paste0('Weather Conditions for ', state.name[match(filtered.data[1,]$StateAbbv,state.abb)])) + 
    ylab('Number of Fatal Car Crashes') + 
    xlab('Weather Conditions')  + 
    guides(fill="none")
  
  return (weather.plot)
}

plot_list <- lapply(unique(merged.df$STUSPS), FUN = weather.bar.plot)

dispal <- colorFactor("Spectral", domain = merged.df$WeatherCondition, na.color = "black")

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = merged.df, 
              fillColor = ~dispal(WeatherCondition), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popupGraph(plot_list, width = 380)) %>%
  addLegend(pal = dispal, 
            values = merged.df$WeatherCondition, 
            position = "bottomright", 
            title = "Weather Conditions") %>%
  fitBounds(-171.791110603, 18.91619, -66.96466, 71.3577635769)
