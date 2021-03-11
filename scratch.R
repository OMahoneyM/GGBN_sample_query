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


# DONE
################################################################################
# --------------------- FUNCTIONIZE THE SCRIPT ABOVE --------------------------#
################################################################################

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

# DONE
################################################################################
# --------------------------- USING SAPPLY ------------------------------------#
################################################################################
# Turning the above code into a function that uses lapply instead of a for loop

# Load in the data containing the taxonomy to query
data <- read.csv('GP_kozloff_edits.csv', header = TRUE, sep = ",")

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

wrapper_test <- function(data_file = NULL, head = NULL, column = NULL) {
# if else check for null values.
  if (is.null(data_file) & is.null(head) & is.null(column)) {
    print("You didn't enter any arguments... Are you even trying to use this function?")
  } else if (is.null(data_file) & is.null(head)) {
    print("data_file argument and head argument are both NULL Enter the correct values for both to proceed with function")
  } else if (is.null(data_file) & is.null(column)) {
    print("data_file argument and column argument are both NULL Enter the correct values for both to proceed with function")
  } else if (is.null(head) & is.null(column)) {
    print("head argument and column argument are both NULL Enter the correct values for both to proceed with function")
  } else if (is.null(data_file)) {
    print("data_file argument is NULL Please supply appropriate data file")
  } else if (is.null(head)) {
    print("head argument is NULL Please indicate the presence of table header with TRUE or FALSE")
  } else if (is.null(column)) {
    print("column argument is NULL Please indicate the header name or numeric column position of the taxonomic names to be queried")
  } else {
    
    if (data_file == 1) {
      data <- data.frame(x = 1:4, y = 5:8, z = 9:12)
    } else if (data_file == 2) {
      data <- "neatwo"
    } else {
      print("Hello. We have been trying to reach you about your car's extended warranty")
    }
    
    if (column == 1) {
      taxa_list <- 
        data[[column]] %>%
        trimws()
    } else {
      print("Please enter TRUE or FALSE for the header argument and check the 
        spelling of the column argument")
    }
  }

}

wrapper_test()
wrapper_test(column = 1)
wrapper_test(head = TRUE)
wrapper_test(data_file = 1)
wrapper_test(head = TRUE, column = 1)
wrapper_test(data_file = 1, column = 1)
wrapper_test(data_file = 1, head = TRUE)
x <- wrapper_test(data_file = 1, head = TRUE, column = 1)

# Load in the data containing the taxonomy to query
data <- read.csv('GP_kozloff_edits_test.csv', header = TRUE, sep = ",")

column <- "scientificname_verbatim"

data4 <- data[[2]]

test_df <- data.frame(uno = 1:4,
                        test_header = 5:8)

test <- "test_header"
test2 <- 2
head <- "poo"

is.logical(head)

if (!is.logical(head)) {
  print("do it right")
} else {
  print("cool")
}

if (grepl("^.*\\.tsv|^.*\\.txt", test) == TRUE) {
  print("success")
} else {
    print("Hello. We have been trying to reach you about your car's extended warranty")
}

if (grepl("^.*\\.csv", data_file) == TRUE) {
  data <- read.table(file = data_file, header = head, sep = ",")
} else if (grepl("^.*\\.tsv|^.*\\.txt", data_file) == TRUE) {
  data <- read.table(file = data_file, header = head, sep = "\t")
} else {
  print("Incorrect data format. Please load .csv, .tsv, or .txt file")
}

is.character(test2)
is.integer(test2)
test2 %% 1 == 0

is.numeric()

if (test %in% names(test_df) | is.numeric(column)) {
  zoop <- "pass"
} else {
  print("Please enter TRUE or FALSE for the header argument and check the spelling of the column argument")
}


if (head == FALSE & is.character(test) == TRUE) {
  print("You entered FALSE for the header argument and a string for the column argument. Please check your file again and re-enter a vaild combination")
} else {
  print("Please enter TRUE or FALSE for the header argument and check the spelling of your column argument")
}
# if column string is found in data_file colnames | column is int
grepl(column, colnames(data_file)) == TRUE | is.integer(column) == TRUE
# else if head == FALSE and column is string

# ---------------------------------------------------------------------------
# QC'd if statements to check if entered arguments are valid
# ---------------------------------------------------------------------------

if (grepl("^.*\\.csv", data_file) == TRUE) {
  data <- read.table(file = data_file, header = head, sep = ",")
} else if (grepl("^.*\\.tsv|^.*\\.txt", data_file) == TRUE) {
  data <- read.table(file = data_file, header = head, sep = "\t")
} else {
  print("Incorrect data format. Please load .csv, .tsv, or .txt file")
}

if (column %in% names(data_file) || is.numeric(column)) {
  taxa_list <- 
    data[[column]] %>%
    trimws()
} else if (head == FALSE && is.character(column) == TRUE) {
  print("You entered FALSE for the header argument and a string for the column 
        argument. Please check your file again and re-enter a vaild combination")
} else {
  print("Please enter TRUE or FALSE for the header argument and check the 
        spelling of the column argument")
}

################################################################################
################# PUTTING EVERYTHING TOGETHER FOR REAL #########################
################################################################################

wrapper_test <- function(data_file = NULL, head = NULL, column = NULL) {
  # Validation check for NULL values function arguments 
  if (is.null(data_file) & is.null(head) & is.null(column)) {
    print("You didn't enter any arguments... Are you even trying to use this function?")
  } else if (is.null(data_file) & is.null(head)) {
    print("data_file argument and head argument are both NULL Enter the correct values for both to proceed with function")
  } else if (is.null(data_file) & is.null(column)) {
    print("data_file argument and column argument are both NULL Enter the correct values for both to proceed with function")
  } else if (is.null(head) & is.null(column)) {
    print("head argument and column argument are both NULL Enter the correct values for both to proceed with function")
  } else if (is.null(data_file)) {
    print("data_file argument is NULL Please supply appropriate data file")
  } else if (is.null(head)) {
    print("head argument is NULL Please indicate the presence of table header with TRUE or FALSE")
  } else if (is.null(column)) {
    print("column argument is NULL Please indicate the header name or numeric column position of the taxonomic names to be queried")
  } else {
    # Validation check on file type for data_file argument and Boolean entry for
    # head argument. If pass the file is loaded as a data frame
    if (!is.logical(head)) {
      print("head argument was provided in incorrect format. Please enter either TRUE or FALSE")
    } else if (grepl("^.*\\.csv", data_file)) {
      data <- read.table(file = data_file, header = head, sep = ",")
    } else if (grepl("^.*\\.tsv|^.*\\.txt", data_file)) {
      data <- read.table(file = data_file, header = head, sep = "\t")
    } else {
      print("Incorrect data format. Please load .csv, .tsv, or .txt file")
    }
    
    # Validation check that column value can be found in header values if it is
    # a string or if it is a numeric value instead. If pass the taxa column is 
    # extracted, white space is trimmed, and stored as taxa_list vector
    if (column %in% names(data_file) || is.numeric(column)) {
      taxa_list <- 
        data[[column]] %>%
        trimws()
      
      # Create new instance of the progress bar with ETA
      pb <- progress_bar$new(
        format = "  downloading [:bar] :percent eta: :eta",
        total = length(taxa_list), clear = FALSE, width= 60)
      
      # The base URL used to query the API
      base <- "http://data.ggbn.org/ggbn_portal/api/search?getSampletype&name="
      
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
      
      return(df)
      
    } else if (head == FALSE && is.character(column) == TRUE) {
      print("You entered FALSE for the head argument and a string for the column argument. Please check your file again and re-enter a vaild combination")
    } else {
      print("Please enter TRUE or FALSE for the head argument and check the spelling of the column argument")
    }

  }
}


df <- wrapper_test(data_file = 'GP_kozloff_edits_test.csv', head = TRUE, column = 2)

# write "result" to TSV
write_tsv(df, 'GGBN_Query_results_Complete.tsv', na = "NA")
