### The following script interacts with Climate Canada's website to scrape Toronto's
### historical daily weather data
### For more information refer to: http://climate.weather.gc.ca/historical_data/search_historic_data_e.html

library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(zoo)
library(data.table)

