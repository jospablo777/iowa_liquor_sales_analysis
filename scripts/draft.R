library(tidyverse)
library(feather)
library(plotly)
library(reticulate)
source('utils/get_data.R')
source('utils/data_processing.R')

# Time series libraries
library(fable)
library(tsibble)
library(feasts)
library(tsibbledata)
library(fabletools)
library(modeltime)
library(timetk)  



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


# lets check the range of time of our data
oldest_entry <- min(iowa_liquor_data$date)
newest_entry <- max(iowa_liquor_data$date)

time_range <- lubridate::interval(oldest_entry, newest_entry)
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

# Let's check how many missing values we have per column
missing_per_column <- colSums(is.na(iowa_liquor_data)) %>% 
  as.list() %>% 
  as_tibble() %>% 
  pivot_longer(all_of(colnames(iowa_liquor_data))) %>% 
  rename(variable=name, missing=value) %>% 
  arrange(desc(missing))
# We have three missing item ids, but the sales fields have no missing values

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
# all the anomalies over the percentile 50%. We are not interested in the lower anomalies 
# because we already know that there are some purchases on weekends, but the
# majority of establishments do not put orders on weekends

percentiles <- seq(from = 0, to = 1, by = 0.05)
percentile_bottles <- quantile(iforest_results$n_bottles, percentiles)

percentile_bottles

anomalies_over_p50_bottles <- iforest_results %>% 
  filter(n_bottles > percentile_bottles['50%'], 
         Anomaly == 1)

# sum(days_more_than_15k_bottles$Anomaly == 1) / (sum(days_more_than_15k_bottles$Anomaly == 1) + sum(days_more_than_15k_bottles$Anomaly == 0))
# 55% are anomalies

# Further analysis
anomalies_over_p50_bottles <- anomalies_over_p50_bottles %>% 
  mutate(month_number     = month(day),
         month_day_number = mday(day),
         year_day_number  = yday(day))


daynumber_by_month <- ggplot(anomalies_over_p50_bottles, aes(x=month_day_number, y=month_number)) +
  geom_point() +
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

daynumber_by_month <- ggplot(anomalies_over_p50_bottles, aes(x=month_day_number, y=month_number)) +
  geom_point() +
  geom_point(data=daynumber_by_month_holidays, aes(x=mean_day, y=mean_month), size=7, color='green', alpha=0.5) +
  geom_text(data = daynumber_by_month_holidays, nudge_x = 0.25, nudge_y = 0.25, 
            check_overlap = T, aes(label=Event, x=mean_day, y=mean_month)) +
  xlab("")

daynumber_by_month
ggplotly(daynumber_by_month)

# Jitter
ggplot(anomalies_over_p50_bottles, aes(x=month_number, y=month_day_number)) +
  geom_jitter() +
  geom_point(data=daynumber_by_month_holidays, aes(x=mean_month, y=mean_day), size=7, color='green', alpha=0.5) +
  geom_text(data = daynumber_by_month_holidays, nudge_x = 0.25, nudge_y = 0.25,
            check_overlap = T, aes(label=Event, x=mean_month, y=mean_day)) +
  xlab("")


ggplot(anomalies_over_p50_bottles, aes(x=year_day_number)) +
  geom_density(color="darkblue", fill="lightblue")
# The more heavy drinking days of the year are concentrated at the end of the year


# -----------------------------------------------------------------
# Time series exploration

consumption_day_summary %>% 
  select(day, n_bottles) %>% 
  mutate(day=ymd(day)) %>% 
  as_tsibble(index=day) %>%
  fill_gaps(n_bottles = 0) %>% # We will assume that for the days with no data are because not purchases occurred, which have sense since most of them are on weekends (i.e. lot of people don't work)
  model(stl = STL(n_bottles)) %>% 
  components() %>% 
  autoplot()
  
# The above is a lot of information, let's aggregate the info in months 
# to analize the chart with less granularity
consumption_month_decomposition <- consumption_day_summary %>% 
  select(day, n_bottles) %>% 
  group_by(month=yearmonth(day)) %>%
  summarise(n_bottles=sum(n_bottles)) %>% 
  as_tsibble(index=month) %>%
  model(stl = STL(n_bottles)) %>% 
  components()  
  
consumption_month_decomposition %>% 
  autoplot() %>% 
  ggplotly()
# Seasonality is interesting across the year we have three peaks, first in June (the more modest), the one in October, and another in December
# We observe a trend increasing in alcohol "consumption" or at least in acquisition by the stores,
# it is improvable that this is due to an increase in the population since it didn' increased that much (show chart).
# Could it be due to an increase in the diversity of available products? Are the stores now able to supply a broader
# spectrum of tastes? Let's see

iowa_liquor_data %>% 
  select(date, itemno) %>% 
  group_by(month=yearmonth(date)) %>% 
  summarise(n_different_products=n_distinct(itemno)) %>% 
  as_tsibble(index=month) %>%
  model(stl = STL(n_different_products)) %>% 
  components() %>% 
  autoplot() %>% 
  ggplotly()
# Ulalà... look at this beauty! Despite the obvious trend in more products entering 
# to the market across time, we can observe that there is an spike in the diversity
# of the available products. Could this be to the proximity to the end of the year 
# festivities? Are the stores exposing the potential consumers to new products
# that the can aquire at the end of the year?

# But yeah the increase in alcoholic beverages could be due to more 
# availability for all the tastes in the market. Maybe I don't like raspberry vodka
# but I love tamarind Smirnoff, the rum with a funny pirate in the bottle, and so on...

# Citation here: Hyndman and Athanasopoulos (2021). Forecasting: Principles and Practice. https://otexts.com/fpp3/

# We observed a trend of an increase in the sales of bottles across the time. 
# Could this be an indicator of population growth?

# Lets check the Iowa population , is related to the increasing
# trend in bottle sales
ggplot(iowa_population, aes(x=Year, y=Population/1e6)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)
# Well... the population has increased in the past 12 years

fit_population_increase <- lm(Population ~ Year, data=iowa_population)
summary(fit_population_increase)
# So, on average the population has increased in 11k persons per year

gv_population_increase <- gvlma::gvlma(fit_population_increase)
summary(gv_population_increase)
# The model seem to be robust, since it complies with the model assumptions


# QUEDE AQUÍ: el análisis de abajo debe de ser ajustado a botellas per capita
# Probar dejando filtrando los nuevos productos a partir de un punto en el tiempo

# -----------------------------------------------------------------
# Lets check if the amount of different products available in the market
# are a good predictor of units sold. Please notice that diversity no only comes
# in flavors, it also comes with sizes and presentations. Let's say an smaller
# bottle of the same rum, or an special tequila edition of your favorite brand
# that includes collective shot glasses

# But  first, now that we know that the population of Iowas have been increasing, and 
# that it could be a potential confounder when we try to estimate the effect of the
# diversity of products, we should to take it into account.
sale_by_diversity <- iowa_liquor_data %>% 
  select(date, itemno, sale_bottles) %>% 
  group_by(month = yearmonth(date)) %>%   # We will explore monthly data in our (available) history
  mutate(year = year(month)) %>% 
  left_join(iowa_population, by=c("year"="Year")) %>% # integrate with Iowa population data
  mutate(population_millions = Population/1e6, # a variable for the population in Millions
         bottles_per_million_inhabitants = sale_bottles/population_millions) %>% # sales of bottles per million inhabitants 
  summarise(n_different_products     = n_distinct(itemno),
            bottles_sale_per_million = sum(bottles_per_million_inhabitants)) 
# Better not to use sale_dollars since the inflation will be another confounder. If we want to
# include dollars, we could adjust for inflation and then remove the effect of this 
# confounder


ggplot(sale_by_diversity, aes(x=n_different_products, y=bottles_sale_per_million)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE)
# Very nice! It seems that the diversity of products is a good predictor. Now, 
# we can get more numerical info from this if we run a linear model

# Linear model
fit_sales_by_diversity <- lm(bottles_sale_per_million ~ n_different_products, data=sale_by_diversity)
summary(fit_sales_by_diversity)


# There are a lot of good tutorials and textbook that dig deeper
# into the validation of linear models, so we will skip that part and use
# gvlma  to check the assumptions in a time saving manner. 

gv_sales_by_diversity <- gvlma::gvlma(fit_sales_by_diversity)
summary(gv_sales_by_diversity)
# The only assumption don't met was the Link Function one. This suggest that the 
# we misspecified the link function, or that we forgot to include an important predictor in 
# our model. You can check more in Peña and Slate (2006) Global Validation of Linear Model Assumptions.
# Since the model fulfill the  Skewness, Kurtosis, and Heteroscedasticity assumptions,
# we will put the link function issue to the side (for now).

# Let's make sense of our model
summary(fit_sales_by_diversity)
# First, we can observe in the R-squared that the model can explain 67% of our data
# variability, i.e., the diversity in alcoholic products is a good predictor,
# over all liquor sale in Iowa.
# Second, we can observe that the estimate for the variable n_different_products is
# 263. We interpret this as follows: with the release of each new product in the market,
# the monthly (alcohol) sales in Iowa will increase in 263 bottles, per million of inhabitants,
# in the State of Iowa

# But how to present this information to non-technical people?

# Let's say that you're working for a liquor distributor, and now you're 
# presenting your findings to the C-level execs. First consider that the 
# expression "will increase in 263 bottles, per million of inhabitants" is too 
# complex. With that in mind, assuming you are in 2027 and that you know that the 
# population then is around 3.8M, you can say something like "For 
# every new product (or new presentation of our old product) we launch in Iowa 
# we will sell around 1000 (i.e., 263.45*3.8) units monthly" # Notice that we say "around 1000" to facilitate the digestion of the info. Round numbers decrease the cognitive load, are easy to remember, and will be easier to assimilate them by your audience.
# Of course it is not that simple, if the product is well liked by the population it will sell
# even more, but if it sucks it wont sell. Also, having more products in the inventory could
# increase costs in logistics, marketing, and so on.


# Presenting this to the gov and health entities. Now let's assume that you're a consultant
# for a local health organization trying to fight the aftermath of excessive drinking. You would
# present the results such as "The increase in diversity makes the alcohol consumption more appealing
# to the consumer, so we can propose an extra tax to the sellers above X amount of different alcoholic products in the inventory. 
# The amount collected will be directed to fund rehabilitation clinics and..."


# Remember that link function assumption violation? Let's go back to that.
# So, we didn't accomplish all the assumptions proposed by Peña and Slate (2006),
# their approach is  quite interesting since the proposed framework includes a 
# global metric, and consider the interaction between assumptions.


# So, lets back to our first model. We will do the same, just with a little difference. 
# Our variables will be log transformed
ggplot(sale_by_diversity, aes(x=log(n_different_products), y=log(bottles_sale_per_million))) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE)
# The plot does not seem that different

# Linear model, log transformed variables
fit_sales_by_diversity_log <- lm(log(bottles_sale_per_million) ~ log(n_different_products), data=sale_by_diversity)
# Check assumptions
gv_sales_by_diversity_log <- gvlma::gvlma(fit_sales_by_diversity_log)
summary(gv_sales_by_diversity_log)
# Great! Now we fulfill our global stat, so we can say that our inference from
# this model will be more robust

# Continuing with the model...
summary(fit_sales_by_diversity_log)

# Still, the model explain 68% of the variability in the data, but now
# the coefficients are different. One of the advantages of the Log-Log regression
# (log dependent variable, log predictor) is its intuitive interpretation: for
# every increase in 1% of X we will have a β% increase (or decrease if β is negative) 
# in our dependent variable. In this case the estimate for β is 0.95, that mean
# that if we increase our diversity of products by 1% we will sell 95% more bottles,
# in other words, we duplicate (x2) the amount of bottles sold every time we 
# diversify in 1% our stock. That's a lot, but again we have to considerate also
# the cost in logistics, space available in our shelf and cellars, supply chain...



# -----------------------------------------------------------------
# Some forecasting for 2024
# This will be good to know in which days will be better to replenish the stores
# check
# https://www.business-science.io/code-tools/2020/06/29/introducing-modeltime.html?utm_content=buffer7ae04&utm_medium=social&utm_source=linkedin.com&utm_campaign=buffer
# QUEDÉ ACÁ


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



