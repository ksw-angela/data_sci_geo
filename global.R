library(dplyr)
library(leaflet)
library(plotly)
library(rgdal)
library(zoo)
library(lubridate)

# For data fidelity and speed instead of a loading the entire script the data file
# was uploaded to github

ksi <- readRDS("www/ksi.rds")
weather <- readRDS("www/weather.rds")

# Function used to capitalize first letter in each word
simpleCap <- function(x) {
  gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE)
}

accidents <- ksi %>%
  filter(ACCLASS != "Property Damage Only") %>%
  mutate(Date = as.Date(as.POSIXct(Date_Time, "GMT"))) %>%
  left_join(weather, by = c("Date" = "Date")) %>%
  mutate(fatal = if_else(ACCLASS == "Fatal", "Yes", "No"),
         STREET1 = simpleCap(STREET1),
         STREET2 = simpleCap(STREET2),
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
                                      ROAD_CLASS)),
         INVAGE = factor(INVAGE, levels = c("0 to 4", "5 to 9", "10 to 14",
                                            "15 to 19", "20 to 24", "25 to 29", 
                                            "30 to 34", "35 to 39", "40 to 44", 
                                            "45 to 49", "50 to 54", "55 to 59",
                                            "60 to 64", "65 to 69", "70 to 74",
                                            "75 to 79", "80 to 84", "85 to 89", 
                                            "90 to 94", "Over 95", "unknown")),
         VEHTYPE = if_else(VEHTYPE == " ", "NA", VEHTYPE),
         DRIVACT = if_else(DRIVACT == " ", "NA", DRIVACT))

# Number of parties involved in an accident as well as number of fatalities
per_accident <- accidents %>%
  group_by(ACCNUM) %>%
  summarize(Involved = n(),
            Fatalities = sum(INJURY == "Fatal"))

accidents <- accidents %>%
  left_join(per_accident, by = c("ACCNUM" = "ACCNUM"))

rm(weather, ksi, per_accident)

# Color scheme for map
pal2 <- colorFactor(palette = c('darkorchid', 'darkturquoise'), domain = accidents$ACCLASS)