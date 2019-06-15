#### Load library ####

library(tidyverse)
library(lubridate)
library(rvest)


#### Historical Toronto-Wide Weather Data ####

# Only use weather stations operated by Environment Canada
base_url <- "http://climate.weather.gc.ca/climate_data/"

url_df <- data.frame(location = c("Toronto City", "Billy Bishop Airport", "North York",
                                 "Pearson Airport", "Buttonville Airport"),
                     station_id = c("31688", "48549", "26953", "51459", "54239"),
                     latitude = c("43.666667","43.627500", "43.780020", "43.676667", "43.860833"),
                     longitude = c("-79.400000","-79.396111", "-79.467788", "-79.630556", "-79.368611"),
                     url = c("?hlyRange=2002-06-04%7C2019-02-09&dlyRange=2002-06-04%7C2019-02-09&mlyRange=2003-07-01%7C2006-12-01&StationID=31688&Prov=ON&urlExtension=_e.html&searchType=stnProx&optLimit=specDate&StartYear=1840&EndYear=2019&selRowPerPage=25&Line=0&txtRadius=50&optProxType=city&selCity=43%7C39%7C79%7C23%7CToronto&selPark=&txtCentralLatDeg=&txtCentralLatMin=0&txtCentralLatSec=0&txtCentralLongDeg=&txtCentralLongMin=0&txtCentralLongSec=0",
                             "?hlyRange=2009-12-10%7C2019-02-09&dlyRange=2010-02-02%7C2019-02-09&mlyRange=%7C&StationID=48549&Prov=ON&urlExtension=_e.html&searchType=stnProx&optLimit=specDate&StartYear=1840&EndYear=2019&selRowPerPage=25&Line=1&txtRadius=50&optProxType=city&selCity=43%7C39%7C79%7C23%7CToronto&selPark=&txtCentralLatDeg=&txtCentralLatMin=0&txtCentralLatSec=0&txtCentralLongDeg=&txtCentralLongMin=0&txtCentralLongSec=0",
                             "?hlyRange=%7C&dlyRange=1994-11-01%7C2019-02-03&mlyRange=1994-01-01%7C2006-12-01&StationID=26953&Prov=ON&urlExtension=_e.html&searchType=stnProx&optLimit=specDate&StartYear=1840&EndYear=2019&selRowPerPage=25&Line=2&txtRadius=50&optProxType=city&selCity=43%7C39%7C79%7C23%7CToronto&selPark=&txtCentralLatDeg=&txtCentralLatMin=0&txtCentralLatSec=0&txtCentralLongDeg=&txtCentralLongMin=0&txtCentralLongSec=0",
                             "?hlyRange=2013-06-11%7C2019-02-09&dlyRange=2013-06-13%7C2019-02-09&mlyRange=%7C&StationID=51459&Prov=ON&urlExtension=_e.html&searchType=stnProx&optLimit=specDate&StartYear=1840&EndYear=2019&selRowPerPage=25&Line=3&txtRadius=50&optProxType=city&selCity=43%7C39%7C79%7C23%7CToronto&selPark=&txtCentralLatDeg=&txtCentralLatMin=0&txtCentralLatSec=0&txtCentralLongDeg=&txtCentralLongMin=0&txtCentralLongSec=0",
                             "?hlyRange=2016-01-13%7C2019-02-09&dlyRange=2018-10-29%7C2019-02-09&mlyRange=%7C&StationID=54239&Prov=ON&urlExtension=_e.html&searchType=stnProx&optLimit=specDate&StartYear=1840&EndYear=2019&selRowPerPage=25&Line=4&txtRadius=50&optProxType=city&selCity=43%7C39%7C79%7C23%7CToronto&selPark=&txtCentralLatDeg=&txtCentralLatMin=0&txtCentralLatSec=0&txtCentralLongDeg=&txtCentralLongMin=0&txtCentralLongSec=0"))


scrape_weather_hourly <- function(date = NULL, base_url = NULL, suffix_url = NULL){
  # Generates hourly weather statistics found in Government of Canada's website
  #
  # Args:
  #   date: start of date interval stored as a Date format e.g. as.Date("2018-01-01")
  #   base_url: string url of Govenment of Canada's climate data
  #   suffix_url: string of url for the particular weather station for hourly statistics
  #
  # Returns:
  #   A dataframe of hourly weather statistics for a specified date and station
  
  station_id <- str_match(suffix_url, "StationID=(.*?)&Prov=ON")[2]
  date <- as.Date(date)
  
  paste0(base_url, "hourly_data_e.html", suffix_url, "&timeframe=1", 
         "&Year=", year(date), "&Month=", month(date), "&Day=", day(date), "#") %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="dynamicDataTable"]/table') %>%
    html_table() %>%
    as.data.frame() %>%
    `colnames<-` (c("Hour", "Temp", "Dew_Point_Temp", "Rel_Humid", 
                    "Wind_Dir", "Wind_Speed", "Visibility", "Pressure", 
                    "Humidex", "Wind_Chill", "Weather_Cond")) %>%
    filter(!is.na(Temp)) %>%
    mutate_all(funs(na_if(., ""))) %>%
    mutate_all(funs(gsub("LegendNANA", NA, 
                         gsub("LegendMM", NA,
                              gsub("LegendTT", NA,
                                   gsub("NA", NA, .)))))) %>%
    mutate(Date = date,
           Hour = substr(Hour, 1, 2) %>% as.numeric(),
           station_id = station_id) %>%
    dplyr::select(station_id, Date, Hour, Temp, Dew_Point_Temp, Rel_Humid,
                  Wind_Dir, Wind_Speed, Visibility, Pressure, Humidex, 
                  Wind_Chill, Weather_Cond)
}

scrape_weather_daily <- function(date = NULL, base_url = NULL, suffix_url = NULL){
  # Generates daily weather statistics found in Government of Canada's website
  #
  # Args:
  #   date: date stored as a Date format e.g. as.Date("2018-01-01")
  #   base_url: string url of Govenment of Canada's climate data
  #   suffix_url: string of url for the particular weather station for daily statistics
  #
  # Returns:
  #   A dataframe of daily weather statistics for the month of specified date and station
  
  station_id <- str_match(suffix_url, "StationID=(.*?)&Prov=ON")[2]
  
  paste0(base_url, "daily_data_e.html", suffix_url, "&timeframe=2", 
         "&Year=", year(date), "&Month=", month(date), "&Day=", day(date), "#") %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="dynamicDataTable"]/table') %>%
    html_table() %>%
    as.data.frame() %>%
    head(-4) %>%
    `colnames<-` (c("Date", "Max_Temp", "Min_Temp", "Mean_Temp", "Heat_Days", "Cool_Days",
                    "Tot_Rain", "Tot_Snow", "Tot_Precip", "Ground_Snow", 
                    "Dir_Max_Gust", "Spd_Max_Gust")) %>%
    filter(Date != "DAY") %>%
    mutate_all(funs(na_if(., ""))) %>%
    mutate_all(funs(gsub("LegendNANA", NA, 
                         gsub("LegendMM", NA,
                              gsub("LegendTT", NA,
                                   gsub("NA", NA, .)))))) %>%
    mutate(station_id = station_id,
           Date = as.Date(paste0(year(date), "-", month(date), "-", Date))) %>%
    dplyr::select(station_id, Date, Max_Temp, Min_Temp, Mean_Temp, Heat_Days, Cool_Days,
                  Tot_Rain, Tot_Snow, Tot_Precip, Ground_Snow, Dir_Max_Gust, Spd_Max_Gust)
}

scrape_weather <- function(start_date = NULL, end_date = start_date,
                           base_url = base_url, suffix_url = NULL, date_type = "hourly"){
  # Generates weather statistics found in Government of Canada's website
  #
  # Args:
  #   start_date: starting date of desired time period (e.g "2018-01-01")
  #   end_date: ending date of desired time period, must be after start_date; if some dates are not,
  #             found function will return as much of the interval that's available
  #   base_url: string url of Govenment of Canada's climate data
  #   suffix_url: string of url for the particular weather station
  #   date_type: frequency of weather statistic; either "hourly" or "daily"
  #
  # Returns:
  #   A dataframe of weather statistics for specified station and frequency and for dates specified
  
  if(as.Date(start_date) > as.Date(end_date)){stop("End date must be after start date")}
  
  else if(date_type == "hourly"){
    min_date <- str_match(suffix_url, "dlyRange=(.*?)%7C")[2]
    max_date <- str_match(suffix_url, "%7C(.*?)&dlyRange=")[2]
    
    if(min_date == "" | max_date == ""){
      return(data.frame(station_id = station_id, Date = NA, Hour = NA, 
                        Temp = NA, Dew_Point_Temp = NA, Rel_Humid = NA,
                        Wind_Dir = NA, Wind_Speed = NA, Visibility = NA, Pressure = NA, 
                        Humidex = NA, Wind_Chill = NA, Weather_Cond = NA))
    }
    else{
      actual_start <- max(as.Date(min_date), as.Date(start_date))
      actual_end <- min(as.Date(max_date), as.Date(end_date))
      
      map_dfr(seq.Date(as.Date(actual_start), as.Date(actual_end), by = "day"), 
              ~scrape_weather_hourly(.x, base_url, suffix_url)) %>%
        mutate(Frequency = "Hourly") 
    }
  }
  
  else if(date_type == "daily"){
    min_date <- str_match(suffix_url, "dlyRange=(.*?)%7C")[2]
    max_date <- str_match(suffix_url, "%7C(.*?)&dlyRange=")[2]
    
    if(min_date == "" | max_date == ""){
      return(data.frame(station_id = station_id, Date = NA, 
                        Max_Temp = NA, Min_Temp = NA, Mean_Temp = NA, Heat_Days = NA, Cool_Days = NA,
                        Tot_Rain = NA, Tot_Snow = NA, Tot_Precip = NA, Ground_Snow = NA, 
                        Dir_Max_Gust = NA, Spd_Max_Gust = NA))
    }
    
    else{
      actual_start <- max(as.Date(min_date), as.Date(start_date))
      actual_end <- min(as.Date(max_date), as.Date(end_date))
      
      if(day(as.Date(actual_start)) <= day(as.Date(actual_end))){
        map_dfr(seq.Date(as.Date(actual_start), as.Date(actual_end), by = "month"),
                ~scrape_weather_daily(.x, base_url, suffix_url)) %>%
          mutate(Frequency = "Daily")
      }
      
      else{
        map_dfr(seq.Date(as.Date(actual_start), 
                         as.Date(paste0(year(as.Date(actual_end)), "-", 
                                        month(as.Date(actual_end)) + 1, "-01")), by = "month"), 
                ~scrape_weather_daily(.x, base_url, suffix_url)) %>%
          mutate(Frequency = "Daily")
      }
    }
  }
}