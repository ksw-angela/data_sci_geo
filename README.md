# Road Safety in Toronto

This is the source code for a Shiny application in R of collisions resulting in serious or fatal accidents in Toronto from 2008-2018. The data is taken directly from the [Toronto Police Service Public Safety Data Portal](http://data.torontopolice.on.ca/datasets/ksi)'s public API. Daily weather data is scraped directly from [Climate Canada](http://climate.weather.gc.ca/historical_data/search_historic_data_e.html)'s website.

To run it locally, you'll need to install the latest versions of tidyverse, Shiny, leaflet, plotly, and sp.

`install.packages(c('tidyverse', 'shiny', 'leaflet', 'plotly', 'sp'))`

You may need to restart R to make sure the newly-installed packages work properly.

After all these packages are installed, you can run this app by entering the directory, and then running the following in R:

`shiny::runApp()`

## Acknowledgments

Thank you to the amazing team in STA2453 for the help and inspiration
