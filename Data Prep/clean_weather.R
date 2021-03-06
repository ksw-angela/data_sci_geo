### The following script interacts with Climate Canada's website to scrape Toronto's
### historical daily weather data for 2008 - last year;
### For more information refer to: http://climate.weather.gc.ca/historical_data/search_historic_data_e.html

library(dplyr)
library(purrr)
library(httr)
library(rvest)

base_url <- "http://climate.weather.gc.ca/climate_data/daily_data_e.html?&timeframe=2&StationID="

last_year <- as.numeric(format(Sys.Date(), "%Y")) - 1

years5097 <- rep(2008:2013, each = 12)
months5097 <- rep(1:12, times = 6)

years51459 <- rep(2014:last_year, each = 12)
months51459 <- rep(1:12, times = last_year - 2014 + 1)

# Climate Canada changes the station ID of Pearson airport from 5097 to 51459 at the
# beginning of 2014

# Station ID 5097
# user  system elapsed 
# 5.50    0.12   50.99 
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

# Station ID 51459
# user  system elapsed 
# 4.85    0.06   50.43 
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
  mutate_at(.vars = vars(-Date),
            .funs = funs(gsub("LegendEE", "",
                              gsub("LegendMM", "",
                                   gsub("LegendTT", "0.0", .))))) %>%
  mutate_at(.vars = vars(-Date, -`Gust Dir`, -`Gust Speed`),
            .funs = funs(as.numeric(.)))

rm(base_url, last_year, years5097, years51459, months5097, months51459, 
   weather_df5097, weather_df51459)