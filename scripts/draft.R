library(tidyverse)
library(feather)
library(plotly)
library(reticulate)
source('utils/get_data.R')
source('utils/data_processing.R')



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
# Extra data

holidays <- read_csv('data/drinking_holidays.csv') %>% 
  mutate(date = as_date(Date, format = '%d-%B-%Y'))

iowa_population <- read_csv('data/iowa_population.csv') %>% 
  select(-Source) %>% 
  mutate(pop_100k=Population/100000)


# -----------------------------------------------------------------
# More variables

# Add the type of spirit/drink
iowa_liquor_data <- iowa_liquor_data %>% 
  mutate(liquor_type = case_when(
    grepl('VODK', category_name)  ~ 'VODKA',
    grepl('WHISK', category_name)  ~ 'WHISKY',
    grepl('RUM', category_name)  ~ 'RUM',
    grepl('SCHN', category_name)  ~ 'SCHNAPPS',
    grepl('TEQ', category_name)  ~ 'TEQUILA',
    grepl('BRANDIE', category_name) | grepl('BRANDY', category_name) ~ 'BRANDY',
    grepl('GIN', category_name)  ~ 'GIN',
    grepl('MEZC', category_name)  ~ 'MEZCAL',
    grepl('CREM', category_name) | grepl('CREAM', category_name) ~ 'CREAM',
    .default = 'OTHER'
  ))

# Add an estimator for the luxury of the spirit
iowa_liquor_data <- iowa_liquor_data %>% 
  mutate(expensiveness = sale_dollars / sale_liters)



# -----------------------------------------------------------------
# EDA: exploratory data analysis


# Some info about the items
luxury_index <- iowa_liquor_data %>% 
  group_by(itemno) %>% 
  summarise(lux=mean(expensiveness),
            sd=sd(expensiveness),
            decription=first(im_desc),
            category_name=first(category_name),
            total_sold_usd=sum(sale_dollars),
            total_sold_liters=sum(sale_liters),
            total_sold_bottles=sum(sale_bottles)) %>% 
  arrange(desc(lux))

aa <- iowa_liquor_data %>% 
  filter(im_desc == "BURNETT'S PINK LEMONADE VODKA MINI")

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


# -----------------------------------------------------------------
# Exploring time series structure
consumption_day_summary <- iowa_liquor_data %>% 
  group_by(day = floor_date(date, unit = "day")) %>% 
  summarise(n_invoices = n(),
            n_bottles  = sum(sale_bottles, na.rm = TRUE),
            liters     = sum(sale_liters, na.rm = TRUE),
            spent_usd  = sum(sale_dollars, na.rm = TRUE)) %>% 
  mutate(Year = year(day)) %>% 
  left_join(iowa_population) %>% 
  mutate(liters_per_100k=liters/pop_100k,
         liters_per_capita=liters/Population,
         week_day = name_days(day),
         weekend = is_weekend(day))

consumption_time_series_day_plot <- ggplot(consumption_day_summary, aes(x=as.Date(day), y=n_bottles)) + #check bottles and liters
  geom_line() + 
  geom_point(aes(color=as.factor(week_day))) +
  # geom_vline(data = holidays %>% filter(Type == 'Health'),
  #            aes(xintercept = as.Date(date)),
  #            size = 0.5, colour = "red") +
  xlab("")

consumption_time_series_day_plot
ggplotly(consumption_time_series_day_plot)
# Friday, Saturday, and Sundays are the days with the less purchases

# TO DO: in each week, determine the day with best and worst purchases. Then, count all the Mondays, Tuesdays, etc. with best/worst sales

anomaly_check <- iowa_liquor_data %>% 
  filter(date %in% c(as_date("2023-10-04"), as_date("2023-10-11"))) %>% 
  arrange(desc(sale_bottles))

consumption_week_summary <- iowa_liquor_data %>% 
  group_by(week = floor_date(date, unit = "week")) %>% 
  summarise(n_invoices = n(),
            n_bottles  = sum(sale_bottles, na.rm = TRUE),
            liters     = sum(sale_liters, na.rm = TRUE),
            spent_usd  = sum(sale_dollars, na.rm = TRUE)) %>% 
  mutate(Year = year(week)) %>% 
  left_join(iowa_population) %>% 
  mutate(liters_per_100k=liters/pop_100k,
         liters_per_capita=liters/Population)

consumption_time_series_week_plot <- ggplot(consumption_week_summary, aes(x=as.Date(week), y=liters_per_100k)) +
  geom_line() + 
  # geom_vline(data = holidays %>% filter(Type == 'Health'),
  #            aes(xintercept = as.Date(date)),
  #            size = 0.5, colour = "red") +
  xlab("")

consumption_time_series_week_plot
ggplotly(consumption_time_series_week_plot)


consumption_month_summary <- iowa_liquor_data %>% 
  group_by(month = floor_date(date, unit = "month")) %>% 
  summarise(n_invoices = n(),
            n_bottles  = sum(sale_bottles, na.rm = TRUE),
            liters     = sum(sale_liters, na.rm = TRUE),
            spent_usd  = sum(sale_dollars, na.rm = TRUE)) %>% 
  mutate(Year = year(month)) %>% 
  left_join(iowa_population) %>% 
  mutate(liters_per_100k=liters/pop_100k,
         liters_per_capita=liters/Population,
         month_ = as.factor(month(month)),
         year_ = as.factor(year(month)))



consumption_time_series_month_plot <- ggplot(consumption_month_summary, aes(x=as.Date(month), y=liters_per_100k, color=as.factor(month_))) +
  geom_line() + 
  # geom_vline(data = holidays %>% filter(Type == 'Health'),
  #            aes(xintercept = as.Date(date)),
  #            size = 0.5, colour = "red") +
  xlab("") 

consumption_time_series_month_plot
ggplotly(consumption_time_series_month_plot)


# summarise months
consumption_month_summary <- iowa_liquor_data %>% 
  group_by(month = floor_date(date, unit = "month")) %>% 
  summarise(n_invoices = n(),
            n_bottles  = sum(sale_bottles, na.rm = TRUE),
            liters     = sum(sale_liters, na.rm = TRUE),
            spent_usd  = sum(sale_dollars, na.rm = TRUE)) %>% 
  mutate(Year = year(month)) %>% 
  left_join(iowa_population) %>% 
  mutate(liters_per_100k=liters/pop_100k,
         liters_per_capita=liters/Population)

# -----------------------------------------------------------------
# Anomaly detection in time series

# Prepare data for our isolation forest
# we will pull this data directly from our python session
data_iforest <- consumption_day_summary %>% 
  select(day, n_bottles, week_day, weekend) %>% 
  mutate(month = month(day, abbr = FALSE, label=TRUE)) # the month will also be a feature

source_python('draft_python.py')

# Retrieve the results from our anomaly detection model
iforest_results <- py$iforest_results %>% 
  mutate(Anomaly = as.factor(Anomaly)) # We will use the anomaly as a factor

# We cannot plot the date as a df index
iforest_results$day <- as.Date(rownames(iforest_results), format = "%Y-%m-%d")


# Let's see the results!
anomalies_plot <- ggplot(iforest_results, aes(x=day, y=n_bottles)) +
  geom_line() + 
  geom_point(aes(color=Anomaly)) +
  xlab("")

anomalies_plot
ggplotly(anomalies_plot)

# -----------------------------------------------------------------
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



