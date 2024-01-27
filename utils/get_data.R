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
# General form to download data in batches

#' Data set batch downloader
#' 
#' `download_iowa_data` downloads specified data from the Iowa Open Data portal (https://data.iowa.gov/).
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
  
  batch_size_str <- format(batch_size, scientific = F)
  n_iterations   <- ceiling(total_of_rows/batch_size)
  
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
      write_csv(df,file_name) # just saves the file
    } else {
      # Creates the directory
      dir.create(file.path(file_final_folder))
      
      # Saves the file
      write_csv(df,file_name) 
    }
    
    # Update offset for next batch
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


# ----------------------------------------------------------------
# General form to read all the downloaded files into a single df
#' Title
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
  folder_path <- paste0(folder_path, '/', data_name, '/')
  filenames <- list.files(folder_path, 
                          pattern=paste0(data_name, ".*\\.csv$"), # Regex to match data_name_XXX.csv
                          full.names=TRUE)
  
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
  # df <- df %>% arrange(date)
  
  return(df)
  
}

