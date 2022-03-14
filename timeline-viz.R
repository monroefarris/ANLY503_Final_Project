# Load Required Libraries ------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, lubridate, leaflet, leafpop, geojsonio, tigris)

# Load Accident Data -----------------------------------------------------------

accident.df <- read.csv('./data/1975/cleaned-accident.csv')
str(accident.df)

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
class(states)