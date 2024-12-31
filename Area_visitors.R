## Use this to start every program.  This clears out previous information from memory
rm(list=ls())

## Initalize renv for library lockfile
library(renv)
#renv::init()

## Packages
#Sys.setenv(RENV_PATHS_RTOOLS = "C:/rtools40/") # https://github.com/rstudio/renv/issues/225

PKG <- c("R.utils","httr","tidyverse","jsonlite","sf","geojsonsf","lwgeom") 

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}
rm(p,PKG)
renv::snapshot()

# Prep tasks to access the API -------------------------------------------------
api_key<-read.csv(file = "APIkey.csv", header = FALSE) 
api_key<-api_key$V1
headers <- add_headers(Authorization = api_key, `Content-Type` = "application/json") # Set up the headers including the API key
url <- "https://api.gravyanalytics.com/v1.1/areas/devices" # API URL to query

# GIS layer
# GIS layer
df<-st_read("Data/PUD_2005-2017_1000m.gpkg") %>% dplyr::select(PUD_YR_AVG)
df<-st_transform(df, crs = 4326)
df<-st_cast(df,"POLYGON") # Switch from multi-polygon to polygon
#df<-st_make_valid(df) # Adjust geometries to right-hand rule (doesn't seem mandatory)

df$startDateTimeEpochMS<-1704067200000
df$endDateTimeEpochMS<-1706831999000
df$returnDeviceCountByGeoHash<-TRUE
df<-df[1:10,] %>% select(-PUD_YR_AVG) # Need more than the geometry column to create a feature collection using sf_geojson
df<-sf_geojson(df,atomise = FALSE) # Convert sf object to GeoJSON 

df<-fromJSON(df) # Doesn't seem to like geojson formatting, switching to json
df<-toJSON(df, auto_unbox = TRUE)

#### Fix below, need to match API returns to specific polygons sent to API

# API call -------------------------------------------------------------
# Export query (asynchronous)
system.time(response <- POST(url, headers, body = df, encode = "json", query = list(
  responseType = "EXPORT"  # Requesting an export response
)))

requestID <- response$headers$requestid
status_url <- paste0("https://api.gravyanalytics.com/v1.1/requestStatus/", requestID)
export_complete <- FALSE

# Function that pings the API to see if the export request is done every 10 seconds and returns either of {files ready, still waiting, failed}
while (!export_complete) {
  Sys.sleep(10)  # Wait for 10 seconds before polling again
  status_response <- GET(status_url, add_headers(Authorization = api_key))
  status_content <- content(status_response, "parsed")
  
  if (status_content$status == "DONE") {
    export_complete <- TRUE
    aws_s3_link <- as.character(status_content$presignedUrlsByDataType$areaVisitorsCounts)
    base::cat("Your files are ready")
  } else if (status_content$status == "FAILED") {
    stop("Export request failed. Please try again.")
  } else {
    base::cat("Export is still in progress. Status:", status_content$status, "\n")
  }
}

# Loading export results into workspace -----------------------------------
# Create data directory if it doesn't exist
if (!dir.exists("Venntel API/R_API_scripts/Data")) {
  dir.create("Venntel API/R_API_scripts/Data", recursive = TRUE)
}

file_name <- sub("\\?.*", "", basename(aws_s3_link)) # Extracting the file name

lapply(seq_along(aws_s3_link), function(i) { # Batch downloading all links returned by the API call. Mode = "wb" is important.
  download.file(aws_s3_link[i], destfile = file.path("Venntel API/R_API_scripts/Data", file_name[i]), mode = "wb")
})

lapply(file_name, function(file_name) { # Unzips the downloaded files (not the whole directory, so safe to use with several serial API calls)
  gunzip(file.path("Venntel API/R_API_scripts/Data", file_name), remove = FALSE, overwrite = TRUE)
})

df<-do.call(rbind, # Row bind files into a dataframe 
            lapply( # Apply over all elements in a list
              file.path("Venntel API/R_API_scripts/Data", sub("\\.gz$", "", file_name)), # Elements in a list that are named based on the API call  
              function(file) {read.csv(file, sep = "|", header = TRUE)})) # Reading files in

