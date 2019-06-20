library(leaflet)
library(plotly)

source("clean_ksi.R")
source("clean_weather.R")

accidents <- ksi %>%
  mutate(Date = as.Date(as.POSIXct(Date_Time, "GMT"))) %>%
  left_join(weather, by = c("Date" = "Date"))

rm(weather, ksi)