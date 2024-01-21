# ================================================================
# Since the dataset is too big, I have trouble retrieving it 
# all simultaneously. Hence, I wrote this function to pull the data 
# in batches. To date, we have a total of 28,176,372 rows.

# The data set can be found in:
# https://data.iowa.gov/Sales-Distribution/Iowa-Liquor-Sales/m3tr-qhgy/about_data

# We use the RSocrata library since the data file is too big, so it is 
# more convenient for us to pull the data through the API
# ================================================================


# ----------------------------------------------------------------
# Download data in batches
download_liquor_data <- function(total_of_rows=28176372, batch_size=1000000) {
  # to the date, we have a total of 28176372 rows
  
  batch_size_str <- format(batch_size, scientific = F)
  n_iterations   <- ceiling(total_of_rows/batch_size)
  
  # we will pull our data in batches of 1 million of rows
  offset <- 0
  
  # Loop to pull the data
  for (i in 1:n_iterations) {
    
    # The API does not like scientific notation in the requests
    offset_str <- format(offset, scientific = F)
    
    # URL
    url <- paste0('https://data.iowa.gov/resource/m3tr-qhgy.csv?$order=:id&$limit=',
                  batch_size_str,
                  '&$offset=', 
                  offset_str)
    
    # Request to the API
    request <- httr::GET(url = url)
    
    # Extract content
    df <- httr::content(request)
    
    # we will save our data locally to save time
    file_name <- paste0('data/data_batch_', i, '.csv')
    write_csv(df,file_name)
    
    offset <- offset + batch_size
    
    cat(paste0('Batch ', i, ' downloaded, ', n_iterations - i, ' are left. \n'))
    
    
  }
}


# ----------------------------------------------------------------
# Read all the downloaded files into a single df
read_liquor_data <- function(folder_path='data') {
  filenames <- list.files(folder_path, pattern="*.csv", full.names=TRUE)
  
  for(i in 1:length(filenames)){
    
    # First read file is used to instantiate the main df
    if (i == 1) {
      df <- read_csv(filenames[i])
      col_specs <- readr::spec(df)
      cat(paste0('Attaching batch ', i, '. \n'))
    } else {
      df_ <- read_csv(filenames[i], col_types=col_specs)
      df <- bind_rows(df, df_)
      cat(paste0('Attaching batch ', i, '. \n'))
    }
    
  }
  
  # sort data by date
  df <- df %>% arrange(date)
  
  return(df)

}


