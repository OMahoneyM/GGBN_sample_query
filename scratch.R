# needed to run fromJSON(), 
library('jsonlite')
# needed to run flatten()
library('purrr')
#library('tidyverse')
# needed to run RCurl()
library('RCurl')
# needed to run rbind.fill()
library('plyr')
# needed to run the progress_bar
library('progress')
# needed to run str_replace_all()
library('stringr')
# needed to run write_tsv()
library('readr')

# The base URL used to query the API
base <- "http://data.ggbn.org/ggbn_portal/api/search?getSampletype&name="

# Load in the data containing the taxonomy to query
data <- read.csv('GP_kozloff_edits.csv', header = TRUE, sep = ",")
# Remove the header from "data"
names(data) <- NULL

# Extract species from "data" and store it as a vector "taxa"
## Note the double brackets "[[]]" in "data[[2]]" as it extracts the column 
## from the data frame and returns it as a vector. You could also write it as 
## "data[,2]" and get the same result. Simply writing "data[2]" returns a 
## a sublist of the "data" with the class data.frame.
test <- c("Hermissenda crassicornis", "Arthropoda", "Polychaeta","Cnidaria", "Annelida", "Echinodermata","Nudibranchia", "Asteroidea", "Nemertea","Nematoda", "Mollusca", "Copepoda")

taxa <- data[[2]]

# trims off leading and trailing whitespace
taxa <- trimws(taxa)

# Create an empty data frame to add the query output to
result <- data.frame()

# Create new instance of the progress bar with ETA
pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = length(taxa), clear = FALSE, width= 60)

# For loop that builds URL, makes GET request, and uses rbind.fill
# to append "request" variable to the data frame "result"
# the flatten() function allows null results to be coerced into dataframes
for (i in 1:length(taxa)) {
  # Initiate progress bar with ETA
  pb$tick(0)
  pb$tick()
  # Query GGBN API and store flattened JSON response in request
  request <- flatten(fromJSON(getURL(URLencode(paste(base,taxa[i], sep = "")))))
  
  # use rbind.fill to append each request to the result dataframe and 
  # autopopulate the null values with NA
  result <- rbind.fill(result, as.data.frame(request))
}

# Remove "fullScientificName_nc=" from the species query
result$filters <- str_replace_all(result$filters, "^.*=", "")

# write "result" to TSV
write_tsv(result, 'GGBN_Query_results_Complete.tsv', na = "NA")


# Done
#-------------------------------------------------------------------------------

# turn above into two functions. one function for the loop 
# and the second function for the data cleanup

GGBN_query <- function(taxa) {
  
  # The base URL used to query the API
  base <- "http://data.ggbn.org/ggbn_portal/api/search?getSampletype&name="
  
  # Create an empty data frame to add the query output to
  result <- data.frame()
  
  # Create new instance of the progress bar with ETA
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = length(taxa), clear = FALSE, width= 60)
  
  # For loop that builds URL, makes GET request, and uses rbind.fill
  # to append "request" variable to the data frame "result"
  # the flatten() function allows null results to be coerced into dataframes
  for (i in 1:length(taxa)) {
    
    # Initiate progress bar with ETA
    pb$tick(0)
    pb$tick()
    
    # Query GGBN API and store flattened JSON response in request
    request <- 
      base %>%
      paste0(taxa[i]) %>%
      URLencode() %>%
      getURL() %>%
      fromJSON() %>%
      flatten()
    
    # use rbind.fill to append each request to the result dataframe and 
    # autopopulate the null values with NA
    result <- 
      request %>%
      as.data.frame() %>%
      rbind.fill(result)
  }
  return(result)
}

df <- GGBN_query(test)
