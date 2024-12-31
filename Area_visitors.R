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
df<-st_cast(df,"POLYGON")
df<-st_make_valid(df)
#df<-lwgeom::st_force_polygon_cw(df)  # Adjust geometries to right-hand rule
#st_is_valid(df) # Checking for self-intersections 

outdata <- st_set_precision(df, precision=10^4)
st_write(outdata, "temp.gpkg")
df <- st_read("temp.gpkg")

df$startDateTimeEpochMS<-1704067200000
df$endDateTimeEpochMS<-1706831999000
df$returnDeviceCountByGeoHash<-TRUE
df<-df[1:2,] %>% select(-PUD_YR_AVG) # Need more than the geometry column to create a feature collection using sf_geojson
df<-sf_geojson(df,atomise = FALSE) # Convert sf object to GeoJSON 

## Is identical to working json_data except 2 features. Maybe out of the study area being on the coast? Maybe too big?


# Accessing API -----------------------------------------------------------
response <- POST(url, headers, body = df, encode = "json", query = list(
  responseType = "EXPORT"  # Requesting an export response
))

