### The following script interacts with the Toronto Police Office's Killed or Seriously Injured
### (KSI) API; A dataset of all traffic collisions where a person was either killed or
### seriously injured in 2008-2018 in Toronto.
### For more information refer to: http://data.torontopolice.on.ca/datasets/ksi

library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)

base_url <- "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/KSI/FeatureServer/0/query?"
end_url <- "&outFields=*&outSR=4326&f=json"

# Toronto Police limits queries to 2000 entries so get one year at a time
ksi_json <- map(2008:2018, 
                ~fromJSON(content(GET(paste0(base_url, "where=YEAR=", .x, end_url)), "text"), 
                          flatten = TRUE))

# Only require the KSI dataset
ksi_df <- map_df(ksi_json, ~bind_rows(.x[["features"]]))

rm(ksi_json)

# Clean final KSI dataset
# 1. Create a single date column
# 2. Attach the ward and hood name
# 3. Remove the geometry columns
# 4. Remove attributes. from column names

# Ward information
ward_info <- read_html("https://www.toronto.ca/city-government/data-research-maps/neighbourhoods-communities/ward-profiles/") %>%
  html_node("#js_map--data") %>%
  html_table() %>%
  select(`Ward Number`, `Ward Name`) %>%
  mutate(`Ward Number` = as.character(`Ward Number`))

# Neighborhood information
hood_info <- fread("https://www.toronto.ca/ext/open_data/catalog/data_set_files/2016_neighbourhood_profiles.csv",
                   nrows = 1) %>%
  select(-Category, -Topic, -`Data Source`, -Characteristic, -`City of Toronto`) %>%
  gather(`Hood Name`, `Hood Number`) %>%
  mutate(`Hood Number` = as.character(`Hood Number`))

ksi <- ksi_df %>%
  select(-starts_with("geometry"))