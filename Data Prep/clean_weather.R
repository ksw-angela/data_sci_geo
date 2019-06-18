### The following script interacts with Climate Canada's website to scrape Toronto's
### historical daily weather data for 2008-2018;
### For more information refer to: http://climate.weather.gc.ca/historical_data/search_historic_data_e.html

library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(zoo)
library(data.table)

base_url <- "http://climate.weather.gc.ca/climate_data/daily_data_e.html?&timeframe=2&StationID="

years5097 <- rep(2008:2013, each = 12)
months5097 <- rep(1:12, times = 6)

years51459 <- rep(2014:2018, each = 12)
months51459 <- rep(1:12, times = 5)

# Climate Canada changes the station ID of Pearson airport from 5097 to 51459 at the
# beginning of 2014

# Station ID 5097 (2008 - 2013)
weather_df5097 <- map2(years5097, months5097,
                       ~read_html(paste0(base_url, "5097&Year=", .x, "&Month=", .y, "#")) %>%
                         html_table(fill = TRUE) %>%
                         bind_rows(.) %>%
                         select(-`Heat Deg Days Definition`, -`Cool Deg Days Definition`)%>%
                         `colnames<-` (c("Date", "Max Temp", "Min Temp", "Mean Temp",
                                         "Tot Rain", "Tot Snow", "Tot Precip",
                                         "Ground Snow", "Gust Dir", "Gust Speed")) %>%
                         # Only include rows with actual data
                         filter(grepl("^[0-9]", Date)) %>%
                         # Create a date column
                         mutate(Date = as.Date(paste0(.x, "-", .y, "-", Date)))) %>%
  bind_rows(.)

# Station ID 51459 (2014 - 2018)
weather_df51459 <- map2(years51459, months51459,
                       ~read_html(paste0(base_url, "51459&Year=", .x, "&Month=", .y, "#")) %>%
                         html_table(fill = TRUE) %>%
                         bind_rows(.) %>%
                         select(-`Heat Deg Days Definition`, -`Cool Deg Days Definition`)%>%
                         `colnames<-` (c("Date", "Max Temp", "Min Temp", "Mean Temp",
                                         "Tot Rain", "Tot Snow", "Tot Precip",
                                         "Ground Snow", "Gust Dir", "Gust Speed")) %>%
                         # Only include rows with actual data
                         filter(grepl("^[0-9]", Date)) %>%
                         # Create a date column
                         mutate(Date = as.Date(paste0(.x, "-", .y, "-", Date)))) %>%
  bind_rows(.)

# Combine to cover entire date period
# LegendEE: Estimated
# LegendMM: Missing
# LegendTT: Trace amounts
weather <- bind_rows(weather_df5097, weather_df51459) %>%
  mutate_at(.vars = vars(`Tot Rain`, `Tot Snow`, `Tot Precip`, `Ground Snow`),
            .funs = funs(gsub("LegendTT", "0", gsub("LegendMM", "", .)))) %>%
  mutate_at(.vars = vars(`Max Temp`, `Min Temp`, `Mean Temp`),
            .funs = funs(gsub("LegendEE", "", gsub("LegendMM", "", .))))