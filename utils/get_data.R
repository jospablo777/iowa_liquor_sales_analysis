# ================================================================
# Data pull utils: Iowa Open Data portal
# 
# Here are some utils to download data from the Iowa Open Data portal.
# Since the data sets can be too big, I have trouble retrieving some of them. it 
# Hence, I wrote this function to pull the data in batches. 
# To date, we have a total of 28,176,372 rows in the liquor sales data set,
# and this can be problematic for some of us.
# We  pull the data iteratively from the API with several GET requests
# 
# The data sets can be found in:
# https://data.iowa.gov/
# 
# Author: José P. Barrantes
# Email: jospablo777@gmail.com
# 
# Date Created: 2024-Jan-20
# ================================================================



# ----------------------------------------------------------------
# Download data in batches

#' Data set batch downloader
#' 
#' `download_iowa_data()` downloads specified data from the Iowa Open Data portal (https://data.iowa.gov/).
#'
#' @param data_id A string. You get it from the data set page in. This can be found on the url of your data set of interest; for example the url for Iowa Fire Department Census data set is "https://data.iowa.gov/Emergency-Management/Iowa-Fire-Department-Census/hv43-6ksq/about_data" here the id is the value nex to the data set name on the right (i.e., "hv43-6ksq").
#' @param folder A string. The path to the folder you will save the data.
#' @param data_name A string. How your files will be called in your file system.
#' @param total_of_rows An integer. The size of the dataset, or the amount of rows you want to pull from the API.
#' @param batch_size An integer. The size of the batch (in rows) you want to pull, the default is 1M rows.
#'
#' @return Nothing. The function save the batches in .csv files in the folder you've chosen for 
#' @export 
#'
#' @examples
#' # Here we download the Iowa Fire Department Census data set. If you go to the dataset web page (https://data.iowa.gov/Emergency-Management/Iowa-Fire-Department-Census/hv43-6ksq/about_data), you will find that the total number of rows of this data set (to the date, 26-Jan-2024) is 738, so if we want the whole data set it is okay if we set total_of_rows=1000, and batch_size=1000.
#' download_iowa_data(data_id='hv43-6ksq', folder='data', data_name='fire_department_census', total_of_rows=1000, batch_size=1000)
download_iowa_data <- function(data_id, folder, data_name, total_of_rows=10000, batch_size=5000) {
  options(readr.show_col_types = FALSE, # Hide readr messages
          readr.show_progress = FALSE)
  
  batch_size_str <- format(batch_size, scientific = F)
  n_iterations   <- ceiling(total_of_rows/batch_size)
  
  # Add a progress bar to track the advance in the data pulling. 
  pb <- txtProgressBar(min = 0,
                       max = n_iterations,
                       style = 3, 
                       width = 50,
                       char = "=") 
  
  # we will pull our data in batches of 1 million of rows
  offset <- 0
  
  # Loop to pull the data
  for (i in 1:n_iterations) {
    
    # The API does not like scientific notation in the requests
    offset_str <- format(offset, scientific = F)
    
    # URL
    url <- paste0('https://data.iowa.gov/resource/',
                  data_id,
                  '.csv?$order=:id&$limit=',
                  batch_size_str,
                  '&$offset=', 
                  offset_str)
    
    # Request to the API
    request <- httr::GET(url = url)
    
    # Extract content
    df <- httr::content(request)
    
    file_final_folder <- paste0(folder, '/', data_name, '/')
    file_name <- paste0(file_final_folder, data_name, '_', i, '.csv')
    
    # We will save our data locally to save time when re-reading the data.
    # But first we must check if the directory for our data exist 
    if (file.exists(file_final_folder)){
      
      write_csv(df, file_name) # just saves the file if directory exists
    } else {
      # Creates the directory
      dir.create(file.path(file_final_folder))
      
      # Saves the file
      write_csv(df, file_name) 
    }
    
    # Update offset for next batch
    offset <- offset + batch_size
    
    # Progress bar update
    setTxtProgressBar(pb, i)
  }
}


# ----------------------------------------------------------------
# General form to read all the downloaded files into a single df

#' Read the data pulled from Iowa Open Data portal (https://data.iowa.gov/).
#' 
#' `read_iowa_data` reads the data pulled from the Iowa Open Data portal API with the function `download_iowa_data`
#'
#' @param folder_path A string. The path to the folder you saved your data.
#' @param data_name A string. How your data set common files are called in your file system.
#'
#' @return A data frame. It reads into the memory the chunks of your data set downloaded in batches from the Iowa Open Data portal (https://data.iowa.gov/).
#' @export
#'
#' @examples
#' # Read the fire_department_census data you downloaded with th download_iowa_data() function. The folder 'data' with the data is located in your R project working directory.
#' read_iowa_data(folder_path='data', data_name='fire_department_census')
read_iowa_data <- function(folder_path='data', data_name) {
  options(readr.show_col_types = FALSE, # hide readr messages
          readr.show_progress = FALSE)
  
  folder_path <- paste0(folder_path, '/', data_name, '/') # Ubicate the specific data set folder
  filenames <- list.files(folder_path, 
                          pattern=paste0(data_name, ".*\\.csv$"), # Regex to match data_name_XXX.csv
                          full.names=TRUE)
  
  # Add a progress bar to track the advance in the data pulling. 
  pb <- txtProgressBar(min = 0,
                       max = length(filenames),
                       style = 3, 
                       width = 50,
                       char = "=") 
  
  for(i in 1:length(filenames)){
    
    # First read file is used to instantiate the main df
    if (i == 1) {
      df <- read_csv(filenames[i])
      col_specs <- readr::spec(df)
    } else {
      df_ <- read_csv(filenames[i], col_types=col_specs)
      df <- bind_rows(df, df_)
    }
    
    # Update progress bar
    setTxtProgressBar(pb, i)
  }
  
  # sort data by date
  # df <- df %>% arrange(date)
  
  return(df)
  
}

