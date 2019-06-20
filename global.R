library(leaflet)
library(plotly)

source("clean_ksi.R")
source("clean_weather.R")

accidents <- ksi %>%
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

# Color scheme for map
pal2 <- colorFactor(palette = c('darkorchid', 'darkturquoise'), domain = accidents$ACCLASS)

# Pop-up label for neighborhood
labs_hood <- paste0('<b>', gsub(" *\\(.*?\\) *", "", neighborhoods$AREA_NAME), '</b>')