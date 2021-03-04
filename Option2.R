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

# The base URL used to query the API
base <- "http://data.ggbn.org/ggbn_portal/api/search?getSampletype&name="

# Load in the data containing the taxonomy to query
data <- read.csv('GP_kozloff_edits.csv', header = TRUE, sep = ",")
# Remove the header from "data"
names(data) <- NULL

taxa <- data[[2]]

# trims off leading and trailing whitespace
taxa <- trimws(taxa)

# Split taxonomy into lists with no more than 213 species
taxa_test <- split(taxa, ceiling(seq_along(taxa)/213))

# Create an empty data frame to add the query output to
result <- data.frame()

for (i in 1:length(taxa_test)) {
  
  # Set variable to store taxa vector   
  n <- taxa_test[[i]]
  
  # Create new instance of the progress bar with ETA
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = length(n), clear = FALSE, width= 60)
  
  for (j in 1:length(n)) {
    # Initiate progress bar with ETA
    pb$tick(0)
    pb$tick()
    
    request <- flatten(fromJSON(getURL(URLencode(paste0(base,n[j])))))
    result <- rbind.fill(result, as.data.frame(request))
  }
  Sys.sleep(10)
}

request <- rbind.fill(result, as.data.frame(flatten(fromJSON(getURL(URLencode(paste(base,n[j], sep = "")), 
                                   .opts = list(ssl.verifypeer = FALSE))))))
result <- rbind.fill(result, as.data.frame(request))
