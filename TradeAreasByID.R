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
url1 <- "https://api.gravyanalytics.com/v1.1/observations/geo/search" # API URL to query
url2 <- "https://api.gravyanalytics.com/v1.1/areas/tradeareas" # API URL to query

# Prep for the GIS layer --------------------------------------------------
if (!dir.exists("tData")) { # Create data directory if it doesn't exist
  dir.create("tData", recursive = TRUE)
}

df<-st_read("Data/PUD_2005-2017_1000m.gpkg") %>% dplyr::select(PUD_YR_AVG) # Load data
df<-st_transform(df, crs = 4326) # Needs to be projected in 4326 to work with lat long conventions of the API
df<-st_cast(df,"POLYGON") # Switch from multi-polygon to polygon
#df<-st_make_valid(df) # Adjust geometries to right-hand rule (doesn't seem mandatory)
df$id<-seq(1,nrow(df),1) # Seems like this needs to be explicitly "id" to pass along to the API to get it to return it as "searchobjectid" in the response psv

# API call -------------------------------------------------------------
batchapi<-function(dft,s,e){ # Function converts sf object to json, passes to api, gets returned data, and merges back with sf object
  
  dft$startDateTimeEpochMS<-s # 1704067200000 (1/1/2024) These don't seem to work as query variables
  dft$endDateTimeEpochMS<-e # 1704672000000 (a week later) 1706831999000 (a year later) 
  dft<-dft %>% select(-PUD_YR_AVG) # Need more than the geometry column to create a feature collection using sf_geojson. Limit of 10 features per request for area observations API
  dftj<-sf_geojson(dft,atomise = FALSE) # Convert sf object to GeoJSON 
  
  dftj<-fromJSON(dftj) # Doesn't seem to like geojson formatting, switching to json
  dftj<-toJSON(dftj, auto_unbox = TRUE, pretty = TRUE)
  
  # Export query (asynchronous)
  system.time(response <- POST(url1, headers, body = dftj, encode = "json", query = list(
    # includeHeaders = FALSE, # Remove headers - potentially useful for batching
    # returnDeviceCountByGeoHash = TRUE, # "If true, the geoHashDeviceCount and geoHashWidthHeights fields are populated per feature" - don't see this. It does return "searchobjectid" in the response psv that corresponds to a given "id" in the json properties
    # exportSchema = "BEST_EVENING_AND_DAYTIME_COMMON_CLUSTERS_PER_DEVICE",
    compressOutputFiles = FALSE, # Compressed outputs?
    responseType = "EXPORT"  # Requesting an export response
  )))
  
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
      aws_s3_link <- as.character(status_content$presignedUrlsByDataType$`observations-geo`)
      base::cat("Your files are ready")
    } else if (status_content$status == "FAILED") {
      stop("Export request failed. Please try again.")
    } else {
      base::cat("Export is still in progress. Status:", status_content$requestDurationSeconds/60,"m", "\n")
    }
  }
  
  # Loading export results into workspace -----------------------------------
  
  file_name <- sub("\\?.*", "", basename(aws_s3_link)) # Extracting the file name
  
  lapply(seq_along(aws_s3_link), function(i) { # Batch downloading all links returned by the API call. Mode = "wb" is important.
    download.file(aws_s3_link[i], destfile = file.path("tData", file_name[i]), mode = "wb")
  })
  
  xp<-do.call(rbind, # Row bind files into a dataframe 
              lapply( # Apply over all elements in a list
                file.path("tData", sub("\\.gz$", "", file_name)), # Elements in a list that are named based on the API call  
                function(file) {read.csv(file, sep = "|", header = TRUE)})) # Reading files in
  
  invisible(file.remove(list.files(path = "tData", full.names = TRUE)))
  
  rid<-as.list(unique(xp$REGISTRATION_ID)) # unique ids seen in the supplied 
  
  # dft$includeRegistrationIDs<-rid[[2]]
  # dftj<-sf_geojson(dft,atomise = FALSE) # Convert sf object to GeoJSON
  # 
  # dftj<-fromJSON(dftj) # Doesn't seem to like geojson formatting, switching to json
  # dftj<-toJSON(dftj, auto_unbox = TRUE, pretty = TRUE)
  
    # Export query (asynchronous)
  system.time(response <- POST(url2, headers, body = dftj, encode = "json", query = list(
    # includeHeaders = FALSE, # Remove headers - potentially useful for batching
    # returnDeviceCountByGeoHash = TRUE, # "If true, the geoHashDeviceCount and geoHashWidthHeights fields are populated per feature" - don't see this. It does return "searchobjectid" in the response psv that corresponds to a given "id" in the json properties
    #decisionLocationTypes = list(c("LATLNG","CBG")),
    decisionLocationTypes = "CBG",
    #includeAdditionalCbgInfo = TRUE,
    #includeGeometryWithCbgInfo = TRUE, # Geometry of CBG for GIS
    exportSchema = "EVENING_COMMON_CLUSTERS",
    compressOutputFiles = FALSE, # Compressed outputs?
    responseType = "EXPORT"  # Requesting an export response
  )))
  
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
      aws_s3_link <- as.character(status_content$presignedUrlsByDataType$tradeAreas)
      base::cat("Your files are ready")
    } else if (status_content$status == "FAILED") {
      stop("Export request failed. Please try again.")
    } else {
      base::cat("Export is still in progress. Status:", round(status_content$requestDurationSeconds/60,2),"m", "\n")
    }
  }
  
  # Loading export results into workspace -----------------------------------
  
  file_name <- sub("\\?.*", "", basename(aws_s3_link)) # Extracting the file name
  
  lapply(seq_along(aws_s3_link), function(i) { # Batch downloading all links returned by the API call. Mode = "wb" is important.
    download.file(aws_s3_link[i], destfile = file.path("tData", file_name[i]), mode = "wb")
  })
  
  xp0<-do.call(rbind, # Row bind files into a dataframe 
              lapply( # Apply over all elements in a list
                file.path("tData", sub("\\.gz$", "", file_name)), # Elements in a list that are named based on the API call  
                function(file) {read.csv(file, sep = "|", header = TRUE)})) # Reading files in
  
  invisible(file.remove(list.files(path = "tData", full.names = TRUE)))
  
  ts<-list(xp,xp0,rid)
  
  return(ts)
}

# Batch locations call to API -------------------------------------------------------
split_dfs<-split(df, ceiling(seq_len(nrow(df))/10)) # Lowest export limit is 10 features in the two APIs queried (area observations and trade areas)

test<-batchapi(split_dfs[[3]],s = 1704067200000, e = 1704672000000)

#xp<-map_df(split_dfs,batchapi, s = 1672531200000, e = 1735689599000) # Batch locations api call, 1/1/2024 - 1704067200000, 12/31/2024 - 1735689599000, 1/1/2023 - 1672531200000, 1/1/2022 - 1640995200000  



