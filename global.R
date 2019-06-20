library(leaflet)
library(plotly)
library(rgdal)
library(zoo)
library(lubridate)

source("clean_ksi.R")
source("clean_weather.R")

accidents <- ksi %>%
  filter(ACCLASS != "Property Damage Only") %>%
  mutate(Date = as.Date(as.POSIXct(Date_Time, "GMT"))) %>%
  left_join(weather, by = c("Date" = "Date")) %>%
  mutate(fatal = if_else(ACCLASS == "Fatal", "Yes", "No"),
         TRAFFCTL = if_else(TRAFFCTL %in% c("Police Control", "School Guard",
                                            "Traffic Controller"), "Human Control",
                            if_else(TRAFFCTL %in% c("Stop Sign", "Traffic Signal",
                                                    "Yield Sign", "Traffic Gate"),
                                    "Traffic Sign",
                                    if_else(TRAFFCTL %in% c("Pedestrian Crossover",
                                                            "Streetcar (Stop for)"),
                                            "Pedestrian Crossing",
                                            "No Traffic Control"))),
         ROAD_CLASS = if_else(ROAD_CLASS %in% c("", "Laneway", "Local", "Other",
                                                "Pending"), "Local",
                              if_else(ROAD_CLASS %in% c("Major Arterial",
                                                        "Minor Arterial"), "Arterial",
                                      ROAD_CLASS)))

rm(weather, ksi)

##### LABELS #####

# Number of parties involved in an accident as well as number of fatalities
per_accident <- accidents %>%
  group_by(ACCNUM) %>%
  summarize(parties_involved = n(),
            num_fatalities = sum(ACCLASS == "Fatal"))

# Color scheme for map
pal2 <- colorFactor(palette = c('darkorchid', 'darkturquoise'), domain = accidents$ACCLASS)