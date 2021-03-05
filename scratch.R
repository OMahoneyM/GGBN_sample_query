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

# ------------------------------------------------------------------------
# Turning the above code into a function that uses lapply instead of a for loop

# Load in the data containing the taxonomy to query
data <- read.csv('GP_kozloff_edits.csv', header = FALSE, sep = ",", skip = 1)

# The base URL used to query the API
base <- "http://data.ggbn.org/ggbn_portal/api/search?getSampletype&name="


# Extract species from "data" and store it as a vector "taxa"
## Note the double brackets "[[]]" in "data[[2]]" as it extracts the column 
## from the data frame and returns it as a vector. You could also write it as 
## "data[,2]" and get the same result. Simply writing "data[2]" returns a 
## a sublist of the "data" with the class data.frame.

# trims off leading and trailing whitespace with trimws()
taxa_list <- 
  data[[2]] %>%
  trimws()

# Create new instance of the progress bar with ETA
pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = length(taxa_list), clear = FALSE, width= 60)

# function that queries GGBN for available sample type data
GGBN_query <- function(taxa) {
    
    # Initiate progress bar with ETA
    pb$tick(0)
    # Update progress bar each time the function is run
    pb$tick()
    
    # Query GGBN API and store flattened JSON response in request
    request <- 
      base %>%
      paste0(taxa) %>%
      URLencode() %>%
      getURL() %>%
      fromJSON() %>%
      flatten() %>%
      as.data.frame()
}

# Run sapply() to query each species in the taxa list using GGBN_query()
df <- 
  taxa_list %>%
  sapply(FUN = GGBN_query)

# Convert df from a list of lists to a data.frame
df <- do.call(rbind.fill, df)

# Remove "fullScientificName_nc=" from the species query
df$filters <- str_replace_all(df$filters, "^.*=", "")

# write "result" to TSV
write_tsv(df, 'GGBN_Query_results_Complete.tsv', na = "NA")

# DONE
################################################################################
# -----------------If/else check on doc type and header -----------------------#
################################################################################
# writing function to check if data inputed was csv, tsv/txt
# also check is data has a header or not

# function(data, header, column) {
# }

# Load in the data containing the taxonomy to query
data <- read.csv('GP_kozloff_edits_test.csv', header = FALSE, sep = ",", skip = 1)

read.table(file = paste0("file.", format), header = header, sep = ",")

read.table(file = paste0("file.", format), header = header, sep = "\t")

test <- "test.csv"

if (grepl("^.*\\.tsv|^.*\\.txt", test) == TRUE) {
  print("success")
} else {
    print("Hello. We have been trying to reach you about your car's extended warranty")
}

if (grepl("^.*\\.csv", test) == TRUE) {
  data <- read.table(file = data, header = header, sep = ",")
} else if (grepl("^.*\\.tsv|^.*\\.txt", test) == TRUE) {
  data <- read.table(file = data, header = header, sep = "\t")
} else {
  print("Incorrect data format Please load .csv, .tsv, or .txt file")
}
