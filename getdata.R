# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)

# Define the base URL from your link
base_url <- "https://portwatch.imf.org/datasets/75619cb86e5f4beeb7dab9629d861acf/api"

# First, let's explore what's available by making a request to the API endpoint
# ArcGIS REST APIs typically return information about the service when accessed directly
explore_api <- GET(base_url)
api_info <- content(explore_api, "text") %>% fromJSON()

# Print the API information
print(api_info)

# Since this is an ArcGIS REST API, we can query features using the "query" endpoint
# The URL structure for feature queries is usually: [base_url]/query
query_url <- paste0(base_url, "/query")

# Set up query parameters
# Common parameters for ArcGIS REST APIs:
# - where: SQL-like filter expression
# - outFields: fields to return (use "*" for all)
# - returnGeometry: whether to return geometry data
# - f: format (json, geojson, etc.)
query_params <- list(
  where = "1=1",  # Get all features
  outFields = "*",  # Get all fields
  returnGeometry = "false",  # We don't need geometry data
  f = "json"  # Request results in JSON format
)

# Make the query request
response <- GET(query_url, query = query_params)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON response
  data <- content(response, "text") %>% fromJSON()
  
  # Print the structure of the data
  str(data)
  
  # If there are features in the response, convert them to a data frame
  if (!is.null(data$features) && length(data$features) > 0) {
    # Extract the attributes from each feature
    port_data <- data$features$attributes
    
    # Print the first few rows
    head(port_data)
    
    # Save the data to a CSV file
    write.csv(port_data, "imf_portwatch_data.csv", row.names = FALSE)
    print("Data saved to 'imf_portwatch_data.csv'")
  } else {
    print("No features returned from the API")
  }
} else {
  print(paste("Error: Status code", status_code(response)))
  print(content(response, "text"))
}

# If you want to limit the number of records, you can use the "resultRecordCount" parameter
# For example, to get only the first 100 records:
limited_query_params <- list(
  where = "1=1",
  outFields = "*",
  returnGeometry = "false",
  resultRecordCount = 100,
  f = "json"
)

# To get data for a specific date range, you can use the "where" parameter
# For example, to get data for a specific date range:
# (Assuming there's a date field called "Date")
date_query_params <- list(
  where = "Date >= '2024-01-01' AND Date <= '2024-04-30'",
  outFields = "*",
  returnGeometry = "false",
  f = "json"
)

# To get data for specific ports, you can filter by port name
# (Assuming there's a field called "PortName")
port_query_params <- list(
  where = "PortName IN ('Tokyo', 'Yokohama', 'Osaka')",
  outFields = "*",
  returnGeometry = "false",
  f = "json"
)

# Function to execute a query with given parameters and return a data frame
get_portwatch_data <- function(params) {
  response <- GET(query_url, query = params)
  
  if (status_code(response) == 200) {
    data <- content(response, "text") %>% fromJSON()
    
    if (!is.null(data$features) && length(data$features) > 0) {
      return(data$features$attributes)
    } else {
      warning("No features returned from the API")
      return(NULL)
    }
  } else {
    warning(paste("Error: Status code", status_code(response)))
    warning(content(response, "text"))
    return(NULL)
  }
}

# Example usage:
# recent_data <- get_portwatch_data(date_query_params)
# japan_ports <- get_portwatch_data(port_query_params)