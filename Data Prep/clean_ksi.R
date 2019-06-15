### The following script interacts with the Toronto Police Office's Killed or Seriously Injured
### (KSI) API; A dataset of all traffic collisions where a person was either killed or
### seriously injured in 2008-2018 in Toronto.
### For more information refer to: http://data.torontopolice.on.ca/datasets/ksi

library(httr)
library(jsonlite)

base_url <- "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/KSI/FeatureServer/0/query?"
end_url <- "where=1%3D1&outFields=*&outSR=4326&f=json"
ksi_json <- fromJSON(content(GET(url), "text"), flatten = TRUE)

ksi <- as.data.frame(ksi_json)