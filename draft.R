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


# mapping zipfiles
# https://catalog.data.gov/dataset/tiger-line-shapefile-2019-2010-nation-u-s-2010-census-5-digit-zip-code-tabulation-area-zcta5-na
# https://stackoverflow.com/questions/70545611/how-to-plot-zipcodes-onto-a-map-of-usa-using-counts-in-r

url <- 'https://data.iowa.gov/resource/m3tr-qhgy.csv?$order=:id&$limit=1000000&$offset=10000'






