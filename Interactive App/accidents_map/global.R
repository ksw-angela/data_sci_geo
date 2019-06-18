## Download this file into local drive: 
## https://github.com/sergiosonline/data_sci_geo/tree/master/data/neighbourhoods_planning_areas_wgs84_SEB
## And update fileloc variable

fileloc <- "C:/Users/angel/OneDrive/Documents/GitHub/data_sci_geo/data/neighbourhoods_planning_areas_wgs84_SEB"

##
##

library(tidyverse, quietly = T, warn.conflicts = F, verbose = F)
library(data.table, quietly = T, warn.conflicts = F, verbose = F)
library(sp)
library(htmltools)
library(leaflet)
library(zoo)
library(lubridate)
library(plotly)

##### DATA CLEANING #####

# Load neighborhood polygons with 2016 population
neighborhoods <- rgdal::readOGR(dsn = fileloc, layer = "NEIGHBORHOODS_WGS84")

# Load accidents data
accidents <- fread("https://raw.githubusercontent.com/sergiosonline/data_sci_geo/master/data/final/accidents.csv") %>%
  filter(acc_class != "Property Damage Only")

# Number of parties involved in an accident as well as number of fatalities
per_accident <- accidents %>%
  group_by(accident_key) %>%
  summarize(parties_involved = n(),
            num_fatalities = sum(injury == "Fatal"))

# 2016 Population for each neighborhood
population <- data.frame(neighborhood = as.numeric(neighborhoods$AREA_S_CD),
                         population_2016 = as.numeric(neighborhoods$X2016pop))

# Combine with per accident information
accidents <- accidents %>%
  left_join(per_accident, by = c("accident_key" = "accident_key")) %>%
  left_join(population, by = c("hood_num" = "neighborhood")) %>%
  select(-division, -ward_num, -hood_num)

rm(per_accident, population)


##### LABELS #####

# Color scheme for map
pal2 <- colorFactor(palette = c('darkorchid', 'darkturquoise'), domain = accidents$acc_class)
pal_pop2 <- colorNumeric(palette = hsv(1, seq(0,1,length.out = 12) , 1), neighborhoods$X2016pop, n = 5)

# Pop-up label for neighborhood
labs_hood <- paste0('<b>', gsub(" *\\(.*?\\) *", "", neighborhoods$AREA_NAME), '</b><br/>',
                    '2016 Population: ', neighborhoods$X2016pop)