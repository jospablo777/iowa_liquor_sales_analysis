library(tidyverse)
library(feather)
source('utils/get_data.R')



# -----------------------------------------------------------------
# Get the data, and some performance considerations

# We will work with the "Iowa Liquor Sales" dataset provided by Iowa Open Data
# Platform. This data is constantly updated, and also is licenced under CC,
# so the use is less restrictive as other kind of data

# Please check utils/get_data.R for more details on the data pull.
# We will do the this only once since from now we will use a local copy of the
# data for speed issues
# download_liquor_data() # call once

# Read locally saved data
# iowa_liquor_data <- read_liquor_data(folder_path = 'data') # call once

# Save the data in a memory friendly (binary) format
# write_feather(iowa_liquor_data, 'data/iowa_liquor_data_full.feather') # save once

# Since feather format is faster we will use this to save time when reading
# the data
iowa_liquor_data <- read_feather('data/iowa_liquor_data_full.feather')



# -----------------------------------------------------------------
# EDA: exploratory data analysis

# lets check the range of time of our data
oldest_entry <- min(iowa_liquor_data$date)
newest_entry <- max(iowa_liquor_data$date)

time_range <- interval(oldest_entry, newest_entry)
time_length(time_range, unit="years")
# So we have 12 years of data, that's a lot!

# Is there any duplicated values in our df?
# Since our dataset is too Big we cannot use duplicated() in a performant way
# so let's check by the invoice
n_distinct(iowa_liquor_data$invoice_line_no) == nrow(iowa_liquor_data) 
# No duplicates were found :)

# How many locations are missing?
sum(is.na(iowa_liquor_data$store_location))
# Around 2.5M of invoices do not have location

no_location_stores <- iowa_liquor_data %>% 
  filter(is.na(store_location)) %>% 
  select(invoice_line_no, date, store, name, zipcode, address) 
# But the missing data is only from 689 stores
n_distinct(no_location_stores$store)

# No location stores, who sells more?
invoices_no_location <- no_location_stores %>% 
  group_by(name) %>% 
  summarise(sales=n(),
            zipcode=first(zipcode),
            address=first(address),
            store=first(store),
            first_sale=min(date),
            last_sale=max(date)) %>% 
  arrange(desc(sales))

# Let's fill manually the missing fields by using google maps
# write_csv(invoices_no_location, 'missing_location_stores.csv')




# mapping zipfiles

# Download shape file
# https://catalog.data.gov/dataset/tiger-line-shapefile-2019-2010-nation-u-s-2010-census-5-digit-zip-code-tabulation-area-zcta5-na
# https://stackoverflow.com/questions/70545611/how-to-plot-zipcodes-onto-a-map-of-usa-using-counts-in-r

# Set timeout to 7 minutes. The shape file we want to download is heavy
options(timeout=60*7)  
# Download the file
# download.file('https://www2.census.gov/geo/tiger/TIGER2019/ZCTA5/tl_2019_us_zcta510.zip',
#               destfile = 'geo_data/tl_2019_us_zcta510.zip')

# Unzip the downloaded file
# system('unzip geo_data/tl_2019_us_zcta510.zip -d geo_data/tl_2019_us_zcta510')

# summarise by zip code
liquor_sales_summary <- iowa_liquor_data %>% 
  group_by(zipcode) %>% 
  summarise(n_stores   = n_distinct(store, na.rm = TRUE),
            n_invoices = n(),
            n_bottles  = sum(sale_bottles, na.rm = TRUE),
            liters     = sum(sale_liters, na.rm = TRUE),
            spent_usd  = sum(sale_dollars, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(zipcode = as.character(zipcode))

# load the zip codes in a simple features table
iowa_zip_codes <- sf::st_read("geo_data/tl_2019_us_zcta510/tl_2019_us_zcta510.shp")


# Put the summary and the zip shape data in a single table
shape_zip_sumary <- iowa_zip_codes %>%
  left_join(liquor_sales_summary, by = c("ZCTA5CE10"="zipcode" )) %>% 
  filter(ZCTA5CE10 %in% liquor_sales_summary$zipcode)

library(sf)
iowa_map <- map_data("county", "iowa")
plot(shape_zip_sumary["liters"])
plot(sf::st_geometry(shape_zip_sumary))



