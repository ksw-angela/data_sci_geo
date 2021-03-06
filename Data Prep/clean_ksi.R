### The following script interacts with the Toronto Police Office's Killed or Seriously Injured
### (KSI) API; A dataset of all traffic collisions where a person was either killed or
### seriously injured in the past 10 years inclusive in Toronto. At time of publishing this
### was from 2008 - 2018.
### For more information refer to: http://data.torontopolice.on.ca/datasets/ksi

library(dplyr)
library(purrr)
library(tidyr)
library(httr)
library(jsonlite)
library(data.table)

base_url <- "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/KSI/FeatureServer/0/query?"
end_url <- "&outFields=*&outSR=4326&f=json"

# Find earliest collision KSI currently has record of
earliest_url <- "where=1%3D1&outFields=*&orderByFields=YEAR&outSR=4326&f=json"
earliest_json <- fromJSON(content(GET(paste0(base_url, earliest_url)), "text"), 
                          flatten = T)
earliest_date <- earliest_json[["features"]] %>%
  summarize(Year = min(`attributes.YEAR`)) %>% 
  as.numeric()

# Find latest collision KSI currently has record of
latest_date <- as.numeric(format(Sys.Date(), "%Y")) - 1

rm(earliest_url, earliest_json)

# Toronto Police limits queries to 2000 entries so get one year at a time
ksi_json <- map(earliest_date:latest_date, 
                ~fromJSON(content(GET(paste0(base_url, "where=YEAR=", .x, end_url)), "text"), 
                          flatten = TRUE))

# Only require the KSI dataset
ksi_df <- map_df(ksi_json, ~bind_rows(.x[["features"]]))

rm(ksi_json, base_url, end_url, earliest_date, latest_date)

# Neighborhood information
hood_info <- fread("https://www.toronto.ca/ext/open_data/catalog/data_set_files/2016_neighbourhood_profiles.csv",
                   nrows = 1) %>%
  select(-Category, -Topic, -`Data Source`, -Characteristic, -`City of Toronto`) %>%
  gather(`Hood Name`, `Hood Number`) %>%
  mutate(`Hood Number` = as.character(`Hood Number`))

# Clean final KSI dataset
# 1. Create a single date column
# 2. Attach hood name
# 3. Remove the geometry columns
# 4. Remove 'attributes.' from column names
ksi <- ksi_df %>%
  mutate(Date_Time = as.POSIXct(attributes.DATE/1000, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(attributes.Hood_ID = as.character(attributes.Hood_ID)) %>%
  left_join(hood_info, by = c("attributes.Hood_ID" = "Hood Number")) %>%
  select(-attributes.YEAR, -attributes.DATE, -attributes.TIME, -attributes.Hour,
         -starts_with("geometry"), -attributes.Ward_ID, -attributes.Ward_Name, 
         -attributes.Hood_Name, -attributes.ObjectId)

names(ksi) <- gsub(x = names(ksi), pattern = "attributes\\.", replacement = "")

rm(hood_info, ksi_df)