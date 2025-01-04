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

# Prep to access the API -------------------------------------------------
api_key<-read.csv(file = "APIkey.csv", header = FALSE) 
api_key<-api_key$V1
headers <- add_headers(Authorization = api_key, `Content-Type` = "application/json") # Set up the headers including the API key
url <- "https://api.gravyanalytics.com/v1.1/areas/devices/trends" # API URL to query

# Prep for the GIS layer --------------------------------------------------
df<-st_read("Data/PUD_2005-2017_1000m.gpkg") %>% dplyr::select(PUD_YR_AVG)
df<-st_transform(df, crs = 4326) # Needs to be projected in 4326 to work with lat long conventions of the API
df<-st_cast(df,"POLYGON") # Switch from multi-polygon to polygon
#df<-st_make_valid(df) # Adjust geometries to right-hand rule (doesn't seem mandatory)

df$startDateTimeEpochMS<-1704067200000 # These don't seem to work as query variables
df$endDateTimeEpochMS<-1706831999000 # endDateTimeEpochMS must be within 90 days from startDateTimeEpochMS disregarding time of day

df$id<-seq(1,nrow(df),1) # Seems like this needs to be explicitly "id" to pass along to the API to get it to return it as "searchobjectid" in the response psv
df<-df[1:20,] %>% select(-PUD_YR_AVG) # Need more than the geometry column to create a feature collection using sf_geojson. Also, there is a limit of 20 features per request (even if it doesn't return results for 20 features).
dft<-sf_geojson(df,atomise = FALSE) # Convert sf object to GeoJSON 

dft<-fromJSON(dft) # Doesn't seem to like geojson formatting, switching to json
dft<-toJSON(dft, auto_unbox = TRUE)

# API call -------------------------------------------------------------
# Export query (asynchronous)
system.time(response <- POST(url, headers, body = dft, encode = "json", query = list(
  #includeHeaders = FALSE, # Remove headers - potentially useful for batching
  returnDeviceCountByGeoHash = TRUE, # "If true, the geoHashDeviceCount and geoHashWidthHeights fields are populated per feature" - don't see this. It does return "searchobjectid" in the response psv that corresponds to a given "id" in the json properties
  compressOutputFiles = FALSE, # Compressed outputs?
  responseType = "EXPORT"  # Requesting an export response
)))

rm(dft)
requestID <- response$headers$requestid
status_url <- paste0("https://api.gravyanalytics.com/v1.1/requestStatus/", requestID)
export_complete <- FALSE

# Function that pings the API to see if the export request is done every 1 seconds and returns either of {files ready, still waiting, failed}
while (!export_complete) {
  Sys.sleep(1)  # Wait for 1 seconds before polling again
  status_response <- GET(status_url, add_headers(Authorization = api_key))
  status_content <- content(status_response, "parsed")
  
  if (status_content$status == "DONE") {
    export_complete <- TRUE
    aws_s3_link <- as.character(status_content$presignedUrlsByDataType$deviceTrends)
    base::cat("Your files are ready")
  } else if (status_content$status == "FAILED") {
    stop("Export request failed. Please try again.")
  } else {
    base::cat("Export is still in progress. Status:", status_content$status, "\n")
  }
}

# Loading export results into workspace -----------------------------------
# Create data directory if it doesn't exist
if (!dir.exists("tData")) {
  dir.create("tData", recursive = TRUE)
}

file_name <- sub("\\?.*", "", basename(aws_s3_link)) # Extracting the file name

lapply(seq_along(aws_s3_link), function(i) { # Batch downloading all links returned by the API call. Mode = "wb" is important.
  download.file(aws_s3_link[i], destfile = file.path("tData", file_name[i]), mode = "wb")
})

xp<-do.call(rbind, # Row bind files into a dataframe 
            lapply( # Apply over all elements in a list
              file.path("tData", sub("\\.gz$", "", file_name)), # Elements in a list that are named based on the API call  
              function(file) {read.csv(file, sep = "|", header = TRUE)})) # Reading files in

invisible(file.remove(list.files(path = "tData", full.names = TRUE)))

xp<-merge(xp,df, by.x = "SEARCHOBJECTID", by.y = "id") %>% dplyr::select(DATE_TYPE, DATE_VALUE, DEVICE_COUNT, geom) %>% filter(DATE_TYPE=="DAY") %>% select(-DATE_TYPE)

### Next steps - batching across ids and time
