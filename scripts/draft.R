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

# Geospatial libraries
library(sf)
library(viridis) # color palette

# -----------------------------------------------------------------
# Get the data, and some performance considerations

# We will work with the "Iowa Liquor Sales" dataset provided by Iowa Open Data
# Platform. This data is constantly updated, and also is licenced under CC,
# so the use is less restrictive as other kind of data

# Please check utils/get_data.R for more details on the data pull.
# We will do the this only once since from now we will use a local copy of the
# data for speed issues
# https://data.iowa.gov/Sales-Distribution/Iowa-Liquor-Sales/m3tr-qhgy/about_data
# call once
# download_iowa_data(data_id       = 'm3tr-qhgy', 
#                    folder        = 'data', 
#                    data_name     = 'iowa_liquor_data', 
#                    total_of_rows = 29000000, # To the date, we have 28,176,372 rows in this data set
#                    batch_size    = 1000000)

# Read locally saved data
# iowa_liquor_data <- read_iowa_data(folder_path='data', data_name='iowa_liquor_data') # call once

# Save the data in a memory friendly (binary) format. Load this file will be much faster.
# write_feather(iowa_liquor_data, 'data/iowa_liquor_data/iowa_liquor_data_full.feather') # save once

# Since feather format is faster we will use this to save time when reading
# the data
iowa_liquor_data <- read_feather('data/iowa_liquor_data/iowa_liquor_data_full.feather')

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
  mutate(
    liquor_type = case_when(
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
      )
    )

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
time_span  <- time_length(time_range, unit="years")
time_span
# So we have 12 years of data, that's a lot!

# Is there any duplicated values in our df?
# Since our dataset is too Big we cannot use duplicated() in a performant way
# so let's check by the invoice
n_distinct(iowa_liquor_data$invoice_line_no) == nrow(iowa_liquor_data) 
# No duplicates were found :)

# How many locations are missing?
missing_location <- sum(is.na(iowa_liquor_data$store_location))
missing_location
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
            liters     = sum(sale_liters, na.rm  = TRUE),
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
consumption_day_summary %>% 
  mutate(day_=ymd(day)) %>% 
  as_tsibble(index=day_) %>%
  fill_gaps(n_bottles = 0)  %>% # Fill with zeroes the days that do not have receipts
  as_tibble() %>% 
  timetk::plot_seasonal_diagnostics(day_, n_bottles)

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
# Now, let's move to a question that to this data science project
# Did de COVID-19 pandemic increased the alcohol consumption of the population?


# There is a great wiki page that summarises all the history line of covid in Iowa. This is a great resource!
# https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Iowa

# Lets create a small table to record the first pandemic important events
events_covid <- c("First cases announcement",
                  "Proclamation of Disaster Emergency",
                  "Order: schools to remain closed through the end of April",
                  "President Biden declares the end to the national emergency")

events_date  <- c("2020-03-08",
                  "2020-03-09",
                  "2020-04-02",
                  "2023-04-10")

event_reach <- c("Iowa",
                 "Iowa",
                 "Iowa",
                 "Nation wide")

events_covid_source <- c("https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Iowa",
                         "https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Iowa",
                         "https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Iowa",
                         "https://en.wikipedia.org/wiki/COVID-19_pandemic_in_the_United_States")

events_color <- c("#8a0303",
                  "#8a0303",
                  "#8a0303",
                  "#4a6741")

iowa_first_covid_events <- tibble(Event  = events_covid, 
                                  Date   = events_date,
                                  Reach  = event_reach,
                                  Source = events_covid_source,
                                  Color  = events_color) %>% 
  mutate(Date = as_date(Date, format = "%Y-%m-%d"))

# We will modify a little bit our products diversity to take into also account all the new products

# First, let's get a vector with all the items available before "2020-04-02"
items_prepandemic <- iowa_liquor_data %>% 
  filter(date < max(iowa_first_covid_events$Date)) %>% 
  dplyr::pull(itemno) %>% 
  unique()

# Then, lets filter our data to exclude any receipt who includes any postpandemic item,
# after that, we can do a montly aggregation of our time serie
sales_in_covid_times <- iowa_liquor_data %>% 
  select(date, itemno, sale_bottles) %>% 
  filter(itemno %in% items_prepandemic) %>% # only prepandemic items
  group_by(month = yearmonth(date)) %>%   # We will explore monthly data in our (available) history
  mutate(year = year(month)) %>% 
  left_join(iowa_population, by=c("year"="Year")) %>% # integrate with Iowa population data
  mutate(population_millions = Population/1e6, # a variable for the population in Millions
         bottles_per_million_inhabitants = sale_bottles/population_millions, # sales of bottles per million inhabitants 
         bottles_per_capita = sale_bottles/Population) %>%  # sales per capita can also be useful
  summarise(n_different_products     = n_distinct(itemno),
            bottles_sale_per_million = sum(bottles_per_million_inhabitants),
            bottles_sale_per_capita  = sum(bottles_per_capita)) %>% 
  ungroup() %>% 
  mutate(year = year(month))

# Plot our time series
ggplot(sales_in_covid_times, aes(x=month, y=bottles_sale_per_million)) +
  geom_line() + 
  xlab("")


# Let's get a little more detailed with our chart and decompose as we did before
sales_in_covid_times %>% 
  as_tsibble(index=month) %>%
  model(stl = STL(bottles_sale_per_million)) %>% 
  components() %>% 
  autoplot() %>% 
  ggplotly()

# Let's check the peak of sales after the adjustment
sales_in_covid_times %>% 
  filter(bottles_sale_per_million == max(bottles_sale_per_million))


# Now you must be thinking "Hey, as new products appear, also old ones disappear. The same year you restricted the emergence of new products, we observe a decrease in sales."
# And you're right, let's put the restriction in new products much before the pandemic and then render a new trend chart

# Vector with only the items available several years before pandemic
items_restriction_before_pandemic <- iowa_liquor_data %>% 
  filter(date < as.Date("2016-01-01")) %>%  # We have chosen the start of 2016. More than four years appart from the pandemic
  dplyr::pull(itemno) %>% 
  unique()

# Generate time series data set
sales_precovid_times <- iowa_liquor_data %>% 
  select(date, itemno, sale_bottles) %>% 
  filter(itemno %in% items_restriction_before_pandemic) %>% # only prepandemic items
  group_by(month = yearmonth(date)) %>%   
  mutate(year = year(month)) %>% 
  left_join(iowa_population, by=c("year"="Year")) %>% # integrate with Iowa population data
  mutate(population_millions = Population/1e6, # a variable for the population in Millions
         bottles_per_million_inhabitants = sale_bottles/population_millions) %>% # sales of bottles per million inhabitants 
  summarise(n_different_products     = n_distinct(itemno),
            bottles_sale_per_million = sum(bottles_per_million_inhabitants)) 

# Plot our time series
ggplot(sales_precovid_times, aes(x=month, y=bottles_sale_per_million)) +
  geom_line() + 
  xlab("")


# Let's get a little more detailed with our chart and decompose as we did before
sales_precovid_times %>% 
  as_tsibble(index=month) %>%
  model(stl = STL(bottles_sale_per_million)) %>% 
  components() %>% 
  autoplot() %>% 
  ggplotly()
# Please notice that the trend is almost the same

# Let's see which now the peak of alcohol purchases
sales_precovid_times %>% 
  filter(bottles_sale_per_million == max(bottles_sale_per_million))
# And there you have it! It is still Dec 2020.
# So we can rule out hte hypothesis of the decrease in sales due to the exclusion 
# of new products while the old ones were drop into the oblivion. If a product is good and sells well, it should remain into the market


# Now back to our sales_in_covid_times dataset.

sales_in_covid_times <- sales_in_covid_times %>% 
  mutate(year = as.factor(year(month)),
         month_number = as.factor(month(month))) # we will add this variable to help us to distinguis between years 


# Filter only the start and the end of the emergency
covid_start_end_dates <- iowa_first_covid_events %>% 
  filter(Date %in% c(min(iowa_first_covid_events$Date), max(iowa_first_covid_events$Date))) %>% 
  mutate(event_summary=if_else(Date == min(Date), 'Start', 'End'))

# In this table we can appreciate the pandemic started at the beginning of 2020, and it ended (by decree) at the start of 2023. If we visualize this we have

sales_in_covid_times %>% 
  as_tsibble(index=month) %>%
  model(stl = STL(bottles_sale_per_million)) %>% 
  components() %>% 
  autoplot() + 
  geom_vline(data = covid_start_end_dates, aes(xintercept = Date, color=event_summary)) +
  scale_color_manual(values=c("#4a6741", "#8a0303")) +
  theme(legend.position="bottom")



# Remember that dataset that considered the sales of only items that existed previous to the pandemic? (sales_in_covid_times) We will use it here
# to test in a formal manner if there was a difference in alcohol sales during the pandemic. First we will 
# prepare each treatment data: pre-pandemic (2017, 2018, and 2019) and 
# during-pandemic (2020, 2021, and 2022). Three years are selected

pre_covid_data <- sales_in_covid_times %>% 
  filter(year %in% c(2017, 2018, 2019)) # Now we have 36 months of aggregated data to compare

during_covid_data <- sales_in_covid_times %>% 
  filter(year %in% c(2020, 2021, 2022)) # Now we have 36 months of aggregated data to compare

library(BEST)
# BEST
# Not available in CRAN anymore
# install.packages('HDInterval') # BEST dependency
# install.packages('https://cran.r-project.org/src/contrib/Archive/BEST/BEST_0.5.4.tar.gz')
# Run an analysis, takes up to 1 min.
# References for this: kind of analysis
# https://jkkweb.sitehost.iu.edu/articles/Kruschke2013JEPG.pdf
# https://link.springer.com/article/10.3758/s13423-016-1221-4
BESTout <- BESTmcmc(during_covid_data$bottles_sale_per_million,
                    pre_covid_data$bottles_sale_per_million, 
                    parallel = FALSE)
                          

# Look at the result:
# TO facilitate the display of results we will modify

mainColor = "skyblue"
dataColor = "red"
comparisonColor = "darkgreen" 
ROPEColor = "darkred"
xlim <- range(BESTout$mu1, BESTout$mu2) #c(0, max(BESTout$mu1, BESTout$mu2))
par(mfrow=c(2,1))
plotPost(BESTout$mu1, 
         xlim = xlim, 
         cex.lab = 1.75, 
         credMass = 0.95, 
         showCurve = FALSE, 
         xlab = bquote(mu[1]), 
         main = paste("During Pandemic", "Mean "), 
         mainColor = mainColor, 
         comparisonColor = comparisonColor, 
         ROPEColor = ROPEColor)
               
plotPost(BESTout$mu2, 
         xlim = xlim, 
         cex.lab = 1.75, 
         credMass = 0.95, 
         showCurve = FALSE, 
         xlab = bquote(mu[2]), 
         main = paste("Pre-pandemic", "Mean"), 
         mainColor = mainColor, 
         comparisonColor = comparisonColor, 
         ROPEColor = ROPEColor)
               
         
par(mfrow=c(1,1))
plotPost(BESTout$mu1 - BESTout$mu2, 
         xlim = c(-0.01, 150000),
         compVal = mean(pre_covid_data$bottles_sale_per_million) * 0.05, # Increase in at least 5%
         showCurve = FALSE, 
         credMass = 0.95, 
         xlab = bquote(mu[1] - mu[2]), 
         cex.lab = 1.75, 
         main = "Difference of Means", 
         mainColor = mainColor, 
         comparisonColor = comparisonColor, 
         ROPEColor = ROPEColor)
               
         
plotPost(BESTout$mu1 / BESTout$mu2, 
         xlim = range(BESTout$mu1 / BESTout$mu2),
         showCurve = FALSE, 
         credMass = 0.95, 
         xlab = bquote(mu[1] / mu[2]), 
         cex.lab = 1.75, 
         main = "Proportional Increase", 
         mainColor = mainColor, 
         comparisonColor = comparisonColor, 
         ROPEColor = ROPEColor)
               



# -----------------------------------------------------------------
# Some forecasting for 2024
# This will be good to know in which days will be better to replenish the stores
# check
# https://www.business-science.io/code-tools/2020/06/29/introducing-modeltime.html?utm_content=buffer7ae04&utm_medium=social&utm_source=linkedin.com&utm_campaign=buffer
# QUEDÉ ACÁ


# -----------------------------------------------------------------
# mapping zip codes

# Iowa open data portal offers the data of the zip codes

# Download Iowa zip codes
# data_id, folder, data_name, total_of_rows=10000, batch_size=5000
# https://data.iowa.gov/dataset/Iowa-Regional-Zip-Codes/gwf2-9cqx/about_data
download_iowa_data(data_id       = 'gwf2-9cqx',      # Taken from the data set url 
                   folder        = 'data',           # Folder name
                   data_name     = 'iowa_zip_codes', # Name of the files
                   total_of_rows = 4000,             # We checked in the web site that the row number was 3,910
                   batch_size    = 4000)


# Now we read the data we just downloaded
iowa_zip_codes <- read_iowa_data(folder_path = 'data', data_name = 'iowa_zip_codes') %>% 
  filter(state_abbrev == 'IA') %>%  # Filter only the zip codes of Iowa since the data contains also zip codes outside Iowa
  select(-point_geom) %>%  # for now, we don't need the centroid of the zip area
  mutate(zcta_code = as.character(zcta_code)) %>% 
  st_as_sf(wkt='the_geom') # We indicate the column with the geometries

# We must integrate our liquor data to the data we just downloaded to get most of
# its value. First, we summarise by zip code
liquor_sales_summary_by_zip <- iowa_liquor_data %>% 
  group_by(zipcode) %>% 
  summarise(n_stores   = n_distinct(store, na.rm = TRUE),
            n_invoices = n(),
            n_bottles  = sum(sale_bottles, na.rm = TRUE),
            liters     = sum(sale_liters, na.rm = TRUE),
            spent_usd  = sum(sale_dollars, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(zipcode = as.character(zipcode))

# Now we integrate our liquor sales data with the map we just downloaded
iowa_zip_codes_ <- iowa_zip_codes %>% 
  left_join(liquor_sales_summary_by_zip, by=c("zcta_code"="zipcode"))

# First visualization
first_map <- iowa_zip_codes_ %>% 
  ggplot() + 
  geom_sf(aes(fill=n_stores)) +
  scale_fill_viridis() + 
  theme_bw()  

ggplotly(first_map)
# Now we can appreciate that we do not have data for every zip code

# In total we have missing data for
sum(!iowa_zip_codes$zcta_code %in% liquor_sales_summary_by_zip$zipcode)
# zip codes in our Iowa liquor data

# Besides the missing data, we can can observe that


# Now let's see if this matches with the most populated cities in Iowa. Lucky us
# we can download another data set from the Iowa Open Data portal
# https://data.iowa.gov/Community-Demographics/Total-City-Population-by-Year/acem-thbp/about_data
download_iowa_data(data_id       = 'acem-thbp',      # Taken from the data set url 
                   folder        = 'data',           # Folder name
                   data_name     = 'iowa_cities_pop', # Name of the files
                   total_of_rows = 40000,            # We checked in the web site that the row number was little more than 31k
                   batch_size    = 40000)

iowa_cities_population <- read_iowa_data(folder_path ='data', data_name='iowa_cities_pop') %>% 
  drop_na(primary_point) %>%    # If we have NA values in our geoms sf won't assimilate the df
  st_as_sf(wkt='primary_point') %>% 
  group_by(geographicname) %>% # let's filter for the last year in every city 
  filter(year == max(year)) 

iowa_most_populated_cities <- iowa_cities_population %>% 
  arrange(desc(population)) %>% # Arrange, most populated first 
  head(20) # Keeps only the most populated 50
# the size of the dot reflects the population size of the city
  

first_map + 
  geom_sf(data=iowa_most_populated_cities,aes(size=population/1000), color='#8a0303') 
# Seems legit. The most populated cities in Iowa are the ones with the most liquor stores

# Now lets check if there is a relation of the "empty" zip code areas with the low population there
iowa_less_populated_cities <- iowa_cities_population %>% 
  arrange(population) %>% # Arrange, less populated first 
  head(600) # Check just 500 cities. Remember that we have around 570 zip codes with no data

first_map + geom_sf(data=iowa_less_populated_cities, aes(), color='#8a0303')

# -----------------------------------------------------------------
# Check how many points fell into the gray area

# First let's isolate the gray area
zip_codes_with_no_recorded_zip_codes <- iowa_zip_codes %>% 
  filter(!zcta_code %in% liquor_sales_summary_by_zip$zipcode) %>% 
  dplyr::pull(the_geom)

iowa_less_populated_cities_points <- iowa_less_populated_cities$primary_point
points_within_gray_area <- st_within(iowa_less_populated_cities_points, zip_codes_with_no_recorded_zip_codes)
as.data.frame(points_within_gray_area)

aa <- st_intersection(iowa_less_populated_cities_points, zip_codes_with_no_recorded_zip_codes)


# -----------------------------------------------------------------
# [CHANGE OF APPROACH]
# Due to the size of this analysis it is unsustainable to do it in a script.
# Hence I will continue it in Quarto