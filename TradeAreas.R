## Use this to start every program.  This clears out previous information from memory
rm(list=ls())

## Initalize renv for library lockfile
library(renv)
#renv::init()

## Packages
#Sys.setenv(RENV_PATHS_RTOOLS = "C:/rtools40/") # https://github.com/rstudio/renv/issues/225

PKG<-c("R.utils","httr","tidyverse","jsonlite","sf","geojsonsf","lwgeom","furrr","arrow","stringr") 

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
headers<-add_headers(Authorization = api_key, `Content-Type` = "application/json") # Set up the headers including the API key
url<-"https://api.gravyanalytics.com/v1.1/areas/tradeareas" # API URL to query

# Prep for the GIS layer --------------------------------------------------
if (!dir.exists("tData")) { # Create data directory if it doesn't exist
  dir.create("tData", recursive = TRUE)
}

df<-st_read("Data/PUD_2005-2017_250m.gpkg") %>% dplyr::select(PUD_YR_AVG) # Load data
df<-st_transform(df, crs = 4326) # Needs to be projected in 4326 to work with lat long conventions of the API
df<-st_cast(df,"POLYGON") # Switch from multi-polygon to polygon
#df<-st_make_valid(df) # Adjust geometries to right-hand rule (doesn't seem mandatory)
df$id<-seq(1,nrow(df),1) # Seems like this needs to be explicitly "id" to pass along to the API to get it to return it as "searchobjectid" in the response psv

# API call -------------------------------------------------------------
batchapi<-function(dft,s,e,fname){ # Function converts sf object to json, passes to api, gets returned data, and merges back with sf object
  
  dft$startDateTimeEpochMS<-s # 1704067200000 These don't seem to work as query variables
  dft$endDateTimeEpochMS<-e # 1706831999000 Says endDateTimeEpochMS must be within 90 days from startDateTimeEpochMS disregarding time of day, but this doesn't seem true. Any date is possible.
  dft$excludeFlags<-25216 # Corresponds to removing
  # dft$startDateTimeEpochMS<-1704067200000
  # dft$endDateTimeEpochMS<-1735689599999
  dft<-dft %>% select(-PUD_YR_AVG) # Need more than the geometry column to create a feature collection using sf_geojson. Also, there is a limit of 20 features per request (even if it doesn't return results for 20 features).
  dftj<-sf_geojson(dft,atomise = FALSE) # Convert sf object to GeoJSON 
  
  dftj<-fromJSON(dftj) # Doesn't seem to like geojson formatting, switching to json
  dftj<-toJSON(dftj, auto_unbox = TRUE)
  
  # Export query (asynchronous)
  system.time(response<-POST(url, headers, body = dftj, encode = "json", query = list(
    # includeHeaders = FALSE, # Remove headers - potentially useful for batching
    # returnDeviceCountByGeoHash = TRUE, # "If true, the geoHashDeviceCount and geoHashWidthHeights fields are populated per feature" - don't see this. It does return "searchobjectid" in the response psv that corresponds to a given "id" in the json properties
    #decisionLocationTypes = list(c("LATLNG","CBG")),
    decisionLocationTypes = "CBG",
    includeAdditionalCbgInfo = TRUE,
    #includeGeometryWithCbgInfo = TRUE, # Geometry of CBG for GIS
    exportSchema = "EVENING_COMMON_CLUSTERS",
    compressOutputFiles = FALSE, # Compressed outputs?
    responseType = "EXPORT"  # Requesting an export response
  )))
  
  requestID<-response$headers$requestid
  status_url<-paste0("https://api.gravyanalytics.com/v1.1/requestStatus/", requestID)
  export_complete<-FALSE
  
  # Function that pings the API to see if the export request is done every 1 seconds and returns either of {files ready, still waiting, failed}
  while (!export_complete) {
    Sys.sleep(1)  # Wait for 1 seconds before polling again
    status_response<-GET(status_url, add_headers(Authorization = api_key))
    status_content<-content(status_response, "parsed")
    
    if (status_content$status == "DONE") {
      export_complete<-TRUE
      aws_s3_link<-as.character(status_content$presignedUrlsByDataType$tradeAreas)
      base::cat("Your files are ready")
    } else if (status_content$status == "FAILED") {
      stop("Export request failed. Please try again.")
    } else {
      base::cat("Export is still in progress. Status:", round(status_content$requestDurationSeconds/60,2),"m", "\n",sep = c(" ","","",""))
    }
  }
  
  # Loading export results into workspace -----------------------------------
  
  file_name<-sub("\\?.*", "", basename(aws_s3_link)) # Extracting the file name
  
  downloaded_files<-lapply(seq_along(aws_s3_link), function(i) { # Batch downloading all links returned by the API call. Mode = "wb" is important.
    file_path<-file.path("tData", file_name[i]) # Construct full path
    download.file(aws_s3_link[i], destfile = file_path, mode = "wb") # Download file
    return(file_path) # Return the file path
  })
  downloaded_files<-unlist(downloaded_files)
  
  xp<-do.call(rbind, # Row bind files into a dataframe 
              lapply( # Apply over all elements in a list
                file.path("tData/", sub("\\.gz$", "", file_name)), # Elements in a list that are named based on the API call  
                function(file) {read.csv(file, sep = "|", header = TRUE)})) # Reading files in
  
  invisible(unlink(downloaded_files)) # Deleting downloaded psv files
  
  if (is.null(xp) || nrow(xp) == 0) { # Handles no data situations where there are no observations in the provided polygon(s)
    warning("No data returned from API for this batch. Skipping...")
    return(NULL)  # Return NULL to avoid stopping execution
  }
  
  #xp<-merge(xp,dft, by.x = "FEATUREID", by.y = "id") # %>% dplyr::select(FEATUREID,DEVICEID,DAY_IN_FEATURE,EARLIEST_OBSERVATION_OF_DAY,LATEST_OBSERVATION_OF_DAY,LATITUDE,LONGITUDE,CENSUS_BLOCK_GROUP_ID,startDateTimeEpochMS,endDateTimeEpochMS,DEVICES_WITH_DECISION_IN_CBG_COUNT,TOTAL_POPULATION)
  xp<-xp %>% dplyr::select(FEATUREID,DEVICEID,DAY_IN_FEATURE,EARLIEST_OBSERVATION_OF_DAY,LATEST_OBSERVATION_OF_DAY,CENSUS_BLOCK_GROUP_ID,DEVICES_WITH_DECISION_IN_CBG_COUNT,TOTAL_POPULATION)
  
  write_parquet(xp, paste0("tData/",fname,".parquet"))
}

# Batch locations call to API -------------------------------------------------------
split_dfs<-split(df, ceiling(seq_len(nrow(df))/20)) # Breaking vector layer dataframe into 20 row subsets held in a list
#test<-batchapi(split_dfs[[1021]],fname = 1, s = 1704067200000, e = 1735689599999)

plan(sequential)
plan(multisession, workers = 2) # Initializing parallel processing, seems like the API can only handle two concurrent connections
set.seed(12)

identify_processed_files<-function(output_dir) { # Identifies indices already downloaded
  files<-list.files(output_dir, pattern = "\\.parquet$", full.names = FALSE) # List all files with .parquet extension
  processed_indices<-str_remove(files, "\\.parquet$") # Extract file names without extensions to get processed indices
  return(processed_indices)
}

# Function to process elements with automatic restart of unprocessed elements
process_batches <- function(t_dfs, output_dir, max_retries = 2) {
  retries <- 0
  unprocessed_indices <- names(t_dfs) # Initial list of all indices
  
  while (retries < max_retries) {
    #cat("Attempting to process the following batches:", unprocessed_indices, "\n")
    cat("Current retry count:", retries, "\n")
    
    # Identify already-processed files
    processed_indices <- identify_processed_files(output_dir)
    unprocessed_indices <- setdiff(names(t_dfs), processed_indices)
    cat("Remaining unprocessed indices:", unprocessed_indices, "\n")
    
    # Process only unprocessed elements
    future_imap(
      t_dfs[unprocessed_indices],
      function(data, index) {
        cat("Processing index:", index, "\n")  # Print the current index
        tryCatch(
          {
            batchapi(data, fname = as.character(index), s = 1704067200000, e = 1735689599999)
          },
          error = function(e) {
            cat(paste("Error processing index", index, ":", e$message, "\n"))
          }
        )
      },
      .options = furrr_options(packages = c("R.utils", "httr", "tidyverse", "jsonlite", 
                                            "sf", "geojsonsf", "lwgeom", "furrr", "arrow"),seed = TRUE),.progress = TRUE
    )
    
    retries <- retries + 1
  }
  
  if (length(unprocessed_indices) > 0) {
    cat("Some batches could not be processed after", max_retries, "retries:", unprocessed_indices, "\n")
  } else {
    cat("All batches processed successfully!\n")
  }
}

system.time(process_batches(split_dfs, output_dir = "tData/"))
#process_batches(split_dfs[399], output_dir = "tData/")

# system.time(
#   xp<-future_imap(split_dfs[900:7000], function(data,index){ # Batch locations api call, 1/1/2024 - 1704067200000, last MS of 12/31/2024 - 1735689599999
#     batchapi(data, fname = as.character(index), s = 1704067200000, e = 1735689599999)
#   },.options = furrr_options(packages = c("R.utils", "httr", "tidyverse", "jsonlite",
#                                           "sf", "geojsonsf", "lwgeom", "furrr", "arrow"))))


#xpt<-map_dfr(list.files("tData/", pattern = "\\.parquet$", full.names = TRUE), read_parquet)

#xp<-map_df(split_dfs,batchapi, s = 1672531200000, e = 1735689599000) # Batch locations api call, 1/1/2024 - 1704067200000, 12/31/2024 - 1735689599000, 1/1/2023 - 1672531200000, 1/1/2022 - 1640995200000  
system.time(xp<-map_df(split_dfs[399],batchapi, s = 1704067200000, e = 1735689599000))


