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

consumption_time_series_day_plot <- 
  ggplot(consumption_day_summary, aes(x=as.Date(day), y=n_bottles)) + 
  geom_line() + 
  xlab("")

consumption_time_series_day_plot
ggplotly(consumption_time_series_day_plot)
# It seems that there are a lot of purchases near to zero. Could all
# the liquor stores agree to not purchase in certain days occasions? 😳


# Let's re-check considering the week days in the chart
consumption_time_series_day_plot <- 
  ggplot(consumption_day_summary, aes(x=as.Date(day), y=n_bottles)) + 
  geom_line() + 
  geom_point(aes(color=as.factor(week_day))) +
  xlab("")

consumption_time_series_day_plot
ggplotly(consumption_time_series_day_plot)
# Friday, Saturday, and Sundays are the days with the less purchases

# TO DO: in each week, determine the day with best and worst purchases. Then, count all the Mondays, Tuesdays, etc. with best/worst sales


# -----------------------------------------------------------------
# Anomaly detection in time series

# Prepare data for our isolation forest
# we will pull this data directly from our python session
data_iforest <- consumption_day_summary %>% 
  select(day, n_bottles, week_day, weekend) %>% 
  mutate(month = month(day, abbr = FALSE, label=TRUE)) # the month will also be a feature

source_python('scripts/anomaly_detection_iforest.py')

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
# It seems that most of the anomalies are those near to zero on weekends (Friday,
# Saturday, and Sunday), this, since more of the businesses are closed on 
# weekends, hence no purchases there, all good. But wait a second what are those in the 4th 
# and the 11th of October 2013, those look suspicious. In fact, lets check all 
# days in which more than 150k bottles were bought

days_more_than_15k_bottles <- iforest_results %>% 
  filter(n_bottles > 150000)

sum(days_more_than_15k_bottles$Anomaly == 1) / (sum(days_more_than_15k_bottles$Anomaly == 1) + sum(days_more_than_15k_bottles$Anomaly == 0))
# 55% are anomalies

# Further analysis
days_more_than_15k_bottles <- days_more_than_15k_bottles %>% 
  mutate(month_number     = month(day),
         month_day_number = mday(day),
         year_day_number  = yday(day))


daynumber_by_month <- ggplot(days_more_than_15k_bottles, aes(x=month_day_number, y=month_number)) +
  geom_point(aes(color=Anomaly)) +
  xlab("")

daynumber_by_month
ggplotly(daynumber_by_month)


daynumber_by_month_holidays <- holidays %>% 
  filter(Type %in% c('Holiday', 'TV')) %>% 
  mutate(month_number = month(date),
         month_day_number   = mday(date)) %>% 
  group_by(Event) %>% 
  summarise(mean_month = mean(month_number),
            mean_day   = mean(month_day_number))

daynumber_by_month <- ggplot(days_more_than_15k_bottles, aes(x=month_day_number, y=month_number)) +
  geom_point(aes(color=Anomaly)) +
  geom_point(data=daynumber_by_month_holidays, aes(x=mean_day, y=mean_month), size=7, color='green', alpha=0.5) +
  geom_text(data = daynumber_by_month_holidays, nudge_x = 0.25, nudge_y = 0.25, 
            check_overlap = T, aes(label=Event, x=mean_day, y=mean_month)) +
  xlab("")

daynumber_by_month
ggplotly(daynumber_by_month)


ggplot(days_more_than_15k_bottles, aes(x=year_day_number)) +
  geom_density(color="darkblue", fill="lightblue")


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



