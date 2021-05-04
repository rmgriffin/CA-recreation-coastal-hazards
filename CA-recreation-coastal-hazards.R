## Use this to start every program.  This clears out previous information from memory
rm(list=ls())

## Initalize renv for library lockfile
library(renv)
#renv::init()

## Packages
#Sys.setenv(RENV_PATHS_RTOOLS = "C:/rtools40/") # https://github.com/rstudio/renv/issues/225

PKG <- c("sf","tidyverse","raster","exactextractr","googledrive", "rgdal","furrr")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}
rm(p,PKG)

## Snapshot of libraries used
renv::snapshot()

## Downloading data
dir.create(file.path('Data'), recursive = TRUE)
folder_url<-"https://drive.google.com/open?id=1mBXEDzc2hBnOa2bxKmg6P9Oz8taR9rTt"
folder<-drive_get(as_id(folder_url))
files<-drive_ls(folder)
dl<-function(files){
  walk(files, ~ drive_download(as_id(.x), overwrite = TRUE))
}
setwd("./Data")
system.time(map(files$id,dl))
system.time(unzip("Data.zip", exdir = "."))
file.remove("Data.zip")
setwd("..")
rm(files, folder, folder_url, dl)

## Data processing function to run across resolutions
data.processing<-function(f,t){

  # Dependent variables
  Flickr<-st_read(f)
  Flickr$PUD<-Flickr$PUD_YR_AVG*13 # Convert PUD_YR_AVG to PUD
  Flickr$id<-seq.int(nrow(Flickr)) # Add id
  Twitter<-st_read(t)
  Flickr$TUD<-Twitter$PUD_YR_AVG*6 # Convert TUD_YR_AVG to TUD
  Depvar<-Flickr[,c("PUD","TUD","id")]
  
  # Independent variables
  ESI<-st_read("Data/ESIL_CA.gpkg") # ESI
  ESI<-st_transform(ESI, st_crs(Depvar))
  ESI<-ESI[,c("ESI","geom")]
  ESIi<-st_intersection(ESI,Depvar)
  ESIi<-ESIi[,c("ESI","geom")]
  ESIi$length<-st_length(ESIi) # Lengths can be greater than the hexagonal size as some shorelines loop considerably within a hexagon
  df<-st_join(Depvar,ESIi) # Spatial join
  df$ESI<-as.character(df$ESI)
  df<-unique(df) # Create a dataframe of only unique values (combinations of classes) within each hexagon
  df<-separate(df,ESI,c("I1","I2","I3"),"/",convert = TRUE) 
  df$x1a<-ifelse(df$I1 == "1A",df$length, ifelse(df$I2 == "1A",df$length, ifelse(df$I3 == "1A",df$length,0))) # Create variables for all classes and populate them with relevant length
  df$x1b<-ifelse(df$I1 == "1B",df$length, ifelse(df$I2 == "1B",df$length, ifelse(df$I3 == "1B",df$length,0)))
  df$x2a<-ifelse(df$I1 == "2A",df$length, ifelse(df$I2 == "2A",df$length, ifelse(df$I3 == "2A",df$length,0)))
  df$x3a<-ifelse(df$I1 == "3A",df$length, ifelse(df$I2 == "3A",df$length, ifelse(df$I3 == "3A",df$length,0)))
  df$x3b<-ifelse(df$I1 == "3B",df$length, ifelse(df$I2 == "3B",df$length, ifelse(df$I3 == "3B",df$length,0)))
  df$x4<-ifelse(df$I1 == "4",df$length, ifelse(df$I2 == "4", 1,ifelse(df$I3 == "4",df$length,0)))
  df$x5<-ifelse(df$I1 == "5",df$length, ifelse(df$I2 == "5", 1,ifelse(df$I3 == "5",df$length,0)))
  df$x6a<-ifelse(df$I1 == "6A",df$length, ifelse(df$I2 == "6A",df$length, ifelse(df$I3 == "6A",df$length,0)))
  df$x7<-ifelse(df$I1 == "7",df$length, ifelse(df$I2 == "7", 1,ifelse(df$I3 == "7",df$length,0)))
  df$x6b<-ifelse(df$I1 == "6B",df$length, ifelse(df$I2 == "6B",df$length, ifelse(df$I3 == "6B",df$length,0)))
  df$x8a<-ifelse(df$I1 == "8A",df$length, ifelse(df$I2 == "8A",df$length, ifelse(df$I3 == "8A",df$length,0)))
  df$x8b<-ifelse(df$I1 == "8B",df$length, ifelse(df$I2 == "8B",df$length, ifelse(df$I3 == "8B",df$length,0)))
  df$x8c<-ifelse(df$I1 == "8C",df$length, ifelse(df$I2 == "8C",df$length, ifelse(df$I3 == "8C",df$length,0)))
  df$x9a<-ifelse(df$I1 == "9A",df$length, ifelse(df$I2 == "9A",df$length, ifelse(df$I3 == "9A",df$length,0)))
  df$x9b<-ifelse(df$I1 == "9B",df$length, ifelse(df$I2 == "9B",df$length, ifelse(df$I3 == "9B",df$length,0)))
  df$x10a<-ifelse(df$I1 == "10A",df$length, ifelse(df$I2 == "10A",df$length, ifelse(df$I3 == "10A",df$length,0)))
  df$xU<-ifelse(df$I1 == "U",df$length, ifelse(df$I2 == "U",df$length, ifelse(df$I3 == "U",df$length,0)))
  df[is.na(df)]<-0 # Replace NAs with zeros
  df<-subset(df, select = c("id","x1a","x1b","x2a","x3a","x3b","x4","x5","x6a","x6b","x7","x8a","x8b","x8c","x9a","x9b","x10a","xU"))
  df$geom<-NULL
  df<-aggregate(. ~ id,df,sum) # Aggregating data by factor (observation is a hexagon, unique by id)
  df<-merge(Depvar,df,"id") # Rejoinging aggregated data to sf object
  df$types<-df$x1a+df$x1b+df$x2a+df$x3a+df$x3b+df$x4+df$x5+df$x6a+df$x6b+df$x7+df$x8a+df$x8b+df$x8c+df$x9a+df$x9b+df$x10a+df$xU 
  df<-df[which(df$types!=0),] # Lots of observations that do not overlap shoreline layer
  rm(Depvar, ESI) # Removing unneeded vars
  
  Pop<-raster("Data/dasymetric_us_20160208.tif") # Adjacent population derived from EPA population raster
  Pop.buff8m<-st_buffer(df,dist=12784) # Creating a (8 mile) buffered shapefile of hexagons
  Pop.buff8m<-st_transform(Pop.buff8m,st_crs(Pop))
  system.time(Pop.buff8m$sumpop<-exact_extract(Pop, Pop.buff8m, "sum")) # Extracting sum of population raster for each buffered polygon 
  Pop.buff8m<-Pop.buff8m[,c("id","sumpop")]  # Merging to dataframe
  Pop.buff8m$geometry<-NULL # Turns sf object into dataframe
  df<-merge(df,Pop.buff8m,"id")
  rm(Pop,Pop.buff8m) # Removing unneeded vars
  df$sumpop<-round(df$sumpop) # Rounding to whole individuals
  
  Rd<-st_read("Data/Roads_2015.gpkg") # Road network distance. Following https://stackoverflow.com/questions/53854803/calculate-minimum-distance-between-multiple-polygons-with-r
  Rd<-st_transform(Rd, st_crs(df))
  system.time(index<-st_nearest_feature(df, Rd)) # Creates an ordered list (index) of closest elements from Rd shapefile to each feature in "df"
  Rd<-Rd %>% slice(index) # Subsets the Rd shapefile by creating a reordered version based on the element ordering of "index"
  df$rdist<-as.vector(st_distance(df, Rd, by_element = TRUE)) # Calculates distance between two sf objects, element-by-element. as.vector removes [m] units
  rm(Rd)
  
  YC<-st_read("Data/coastal_access_locations.gpkg") # Adding "Yourcoast" data from California Coastal Commission and postprocessing. Nearest calculation follows same process as road network approach
  YC<-st_transform(YC,st_crs(df))
  index<-st_nearest_feature(df, YC) # Creates an ordered list (index) of closest elements from YC shapefile to each feature in "df"
  YC<-YC %>% slice(index) # Subsets the YC shapefile by creating a reordered version based on the element ordering of "index"
  df$YCdist<-as.vector(st_distance(df, YC, by_element = TRUE)) # Calc dist between two sf objects, element-by-element; as.vector removes [m] units
  df<-cbind(df,YC) # Column binding
  rm(YC)
  df<-subset(df, select = -c(BLFTP_PRK, BLFTP_TRLS, BLUFF, DOG_FRIEND, DUNES, EZ4STROLLE, PCNC_AREA, RKY_SHORE, SNDY_BEACH, TIDEPOOL, VISTOR_CTR, VOLLEYBALL, WLDLFE_VWG, COUNTY, CountyNum, DISTRICT, Descriptio, GEOGR_AREA, ID, LATITUDE, LONGITUDE, LIST_ORDER, LocationMo, NameMobile, PHONE_NMBR, Photo_1, Photo_2, Photo_3, Photo_4, Bch_whlchr, geom)) # Dropping variables we are not going to use in the analysis (see google doc)
  df<-rapply(df, as.character, classes="factor", how="replace") # Factors as character https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
  df <- df %>% # # all "no" to zero, and all "yes" to 1 for indicator variables, Note, there are a range of values here {?, No, Yes, Yes?, yes, no, NO} and we recode these as {0, 0, 1, 1, 1, 0, 0} where 1 == yes and 0 == no.
    mutate_if(is.character,funs(str_replace(., c("\\?|No|NO|no"), "0"))) # http://www2.stat.duke.edu/~cr173/Sta523_Fa15/regex.html
  df <- df %>% 
    mutate_if(is.character,funs(str_replace(., c("Yes|yes|Yes\\?"), "1")))
  table(df$BT_FACIL_T) # Boating facilities (is different from "BOATING", though mostly overlap). An array of facilities, assign 1 if something, 0 if nothing
  df$BT_FACIL_T<-ifelse(is.na(df$BT_FACIL_T),0,ifelse(df$BT_FACIL_T==0,0,1))
  df$BIKE_PATH<-ifelse(df$BIKE_PATH==10,1,df$BIKE_PATH)  # Bike path seems to have a coding error (10 instead of 1)
  df<-rapply(df, as.numeric, classes="character", how="replace") # Character as numeric
  sapply(df, function(x) sum(is.na(x))) # Any NAs? 
  df[is.na(df)]<-0 # Replace NAs with zero
  
  WT<-st_read("Data/WaterTrailSites_July2016.gpkg") # Water trail (existing sites as of 2016)
  WT<-WT[,c("WaterTrail","geom")]
  WT<-st_transform(WT,st_crs(df))
  df<-st_join(df,WT)# Spatial join
  df$WaterTrail<-ifelse(is.na(df$WaterTrail),0,df$WaterTrail)
  rm(WT)
  
  BT<-st_read("Data/Mar_2018_BayTrail.gpkg")  # Bay trail
  BT<-BT[which(BT$STATUS=="Existing"),] # Only currently existing projects
  BT$Year_Compl<-as.character(BT$Year_Compl)
  BT$Year_Compl<-ifelse(is.na(BT$Year_Compl),1989,
                        ifelse(BT$Year_Compl=="pre-1989",1989,BT$Year_Compl)) # Assuming all NAs are 1989 or prior
  BT$Year_Compl<-as.numeric(BT$Year_Compl)
  #BT<-BT[which(BT$YearCompl<=2004),] # Filter by year here
  BT$BayTrail<-1 # Indicator variable
  BT<-BT[,c("BayTrail","geom")]
  df<-st_join(df,BT) # Spatial join
  df<-unique(df) # Two or more lines may exist within one hexagon - only need one
  df$BayTrail[is.na(df$BayTrail)]<-0 # Replacing NAs with zero
  rm(BT)
  
  DM<-st_read("Data/nlcd_wetlands_2016.gpkg") # Distance to nearest marsh. Follows similar approach to road nearest distance.
  DM<-st_transform(DM, st_crs(df))
  system.time(index<-st_nearest_feature(df, DM)) # Creates an ordered list (index) of closest elements from Rd shapefile to each feature in "df"
  DM<-DM %>% slice(index) # Subsets the Rd shapefile by creating a reordered version based on the element ordering of "index"
  df$wtlddist<-as.vector(st_distance(df, DM, by_element = TRUE)) # Calculates distance between two sf objects, element-by-element. as.vector removes [m] units
  rm(DM, index)
  
  rs.f<-list.files("./Data/Prism Precip", full.names = TRUE) #Adjacent precipitation
  rs.t<-rs.f[8:13]
  s.f<-stack(rs.f)
  s.t<-stack(rs.t)
  Prec.buff8m<-st_buffer(df,dist=12784) # Creating a (8 mile) buffered shapefile of hexagons 
  Prec.buff8m<-st_transform(Prec.buff8m,crs=crs(s.f))
  Prec.buff8m<-subset(Prec.buff8m, select = c(id,geometry))
  system.time(Prec.buff8m$meanprecf<-rowMeans(exact_extract(s.f, Prec.buff8m, "mean"))) # Extracting mean precip for each buffered polygon 
  system.time(Prec.buff8m$meanprect<-rowMeans(exact_extract(s.t, Prec.buff8m, "mean")))
  Prec.buff8m<-Prec.buff8m[,c("id","meanprecf","meanprect")]  
  Prec.buff8m$geometry<-NULL # Turns sf object into dataframe
  df<-merge(df,Prec.buff8m,"id") # Merging to dataframe
  rm(s.f,s.t,Prec.buff8m,rs.f,rs.t) # Removing unneeded vars
  
  rs.f<-list.files("./Data/Prism Mean Temp", full.names = TRUE) # Adjacent air temperature
  rs.t<-rs.f[8:13]
  s.f<-stack(rs.f)
  s.t<-stack(rs.t)
  AT.buff8m<-st_buffer(df,dist=12784) # Creating a (8 mile) buffered shapefile of hexagons
  AT.buff8m<-st_transform(AT.buff8m,crs=crs(s.f))
  AT.buff8m<-subset(AT.buff8m, select = c(id,geometry))
  system.time(AT.buff8m$meanatf<-rowMeans(exact_extract(s.f, AT.buff8m, "mean"))) # Extracting mean air temp for each buffered polygon 
  system.time(AT.buff8m$meanatt<-rowMeans(exact_extract(s.t, AT.buff8m, "mean")))
  AT.buff8m<-AT.buff8m[,c("id","meanatf","meanatt")]
  AT.buff8m$geometry<-NULL # Turns sf object into dataframe
  df<-merge(df,AT.buff8m,"id") # Merging to dataframe
  rm(AT.buff8m,s.f,s.t,rs.f,rs.t) # Removing unneeded vars
  
  SSTf<-raster("Data/flickr_GIOVANNI-g4.timeAvgMap.MODISA_L3m_SST_Monthly_4km_R2019_0_sst.20050101-20171231.125W_32N_116W_42N.tif") # Adjacent sea surface temperature
  SSTt<-raster("Data/twitter_GIOVANNI-g4.timeAvgMap.MODISA_L3m_SST_Monthly_4km_R2019_0_sst.20120101-20171231.125W_32N_116W_42N.tif")
  SST.buff<-st_buffer(df,dist=15000) # Creating a (15km) buffered shapefile of hexagons
  SST.buff<-st_transform(SST.buff,crs=crs(SSTf))
  system.time(SST.buff$meanSSTf<-exact_extract(SSTf, SST.buff, "mean")) # Extracting mean sea surface temp for each buffered polygon
  system.time(SST.buff$meanSSTt<-exact_extract(SSTt, SST.buff, "mean"))
  SST.buff<-SST.buff[,c("id","meanSSTf","meanSSTt")] 
  SST.buff$geometry<-NULL # Turns sf object into dataframe
  df<-merge(df,SST.buff,"id") # Merging to dataframe
  rm(SST.buff,SSTf,SSTt) # Removing unneeded vars
  
  # Transferring to long format with flickr/twitter id (PUD vs TUD)
  df<-gather(df,source,ud,c(PUD,TUD))
  df$meanprec<-ifelse(df$source=="PUD",df$meanprecf,df$meanprect)
  df$meanat<-ifelse(df$source=="PUD",df$meanatf,df$meanatt)
  df$meanSST<-ifelse(df$source=="PUD",df$meanSSTf,df$meanSSTt)
  df<-subset(df, select = -c(meanprecf,meanprect,meanatt,meanatf,meanSSTt,meanSSTf))
  
  return(df)
}

## Processing for all resolutions
# Flickr data
file.f<-c("./Data/PUD_2005-2017_1000m.gpkg",
          "./Data/PUD_2005-2017_500m.gpkg",
          "./Data/PUD_2005-2017_250m.gpkg")
# Twitter data
file.t<-c("./Data/TUD_2012-2017_1000m.gpkg",
          "./Data/TUD_2012-2017_500m.gpkg",
          "./Data/TUD_2012-2017_250m.gpkg")

# Multicore implementation
plan(multisession, workers = 3)
# Executing data processing function
system.time(results<-future_map2_dfr(file.f,file.t,data.processing,.id="hres")) # 1671.561s 
rm(file.f,file.t)
# Renaming hres id to reflect resolution
results$hres<-ifelse(results$hres==1, 1000,
                     ifelse(results$hres==2, 500, 250))

## Attaching cell data
dc<-st_read("./Data/DC2019.gpkg")
dc<-st_transform(dc,st_crs(results)) # reprojecting
dc<-dc %>% dplyr::select(DC_YR,geom) # only annual sum

results2<-st_join(results,dc)
rm(dc)

results2<-results2 %>% 
  group_by(hres, id) %>% 
  mutate(ud = sum(DC_YR)) %>% 
  dplyr::select(-c(DC_YR,meanprec,meanat,meanSST)) %>% 
  mutate(source = "DC") %>% 
  unique()

# Adjacent precipitation cell data
Prec.buff8m<-st_buffer(results2,dist=12784) # Creating a (8 mile) buffered shapefile of hexagons 
prec<-raster("./Data/Prism Precip/PRISM_ppt_stable_4kmM3_2019.tif")
Prec.buff8m<-st_transform(Prec.buff8m,st_crs(prec))
Prec.buff8m<-subset(Prec.buff8m, select = c(id,geometry,hres))
system.time(Prec.buff8m$meanprec<-exact_extract(prec, Prec.buff8m, "mean")) # Extracting mean precip for each buffered polygon 
Prec.buff8m$geometry<-NULL # Turns sf object into dataframe
results2<-left_join(results2,Prec.buff8m,by = c("id","hres"))
rm(Prec.buff8m,prec) # Removing unneeded vars

# Adjacent air temperature cell data
AT.buff8m<-st_buffer(results2,dist=12784) # Creating a (8 mile) buffered shapefile of hexagons
at<-raster("./Data/Prism Mean Temp/PRISM_tmean_stable_4kmM3_2019.tif")
AT.buff8m<-st_transform(AT.buff8m,st_crs(at))
AT.buff8m<-subset(AT.buff8m, select = c(id,geometry,hres))
system.time(AT.buff8m$meanat<-exact_extract(at, AT.buff8m, "mean")) # Extracting mean air temp for each buffered polygon 
AT.buff8m$geometry<-NULL # Turns sf object into dataframe
results2<-left_join(results2,AT.buff8m,by = c("id","hres"))
rm(AT.buff8m,at) # Removing unneeded vars

# Adjacent sea surface temperature cell data
SST<-raster("./Data/Cell_GIOVANNI-g4.timeAvgMap.MODISA_L3m_SST_Monthly_4km_R2019_0_sst.20190101-20191231.124W_32N_117W_42N.tif")
SST.buff<-st_buffer(results2,dist=15000) # Creating a (15km) buffered shapefile of hexagons
SST.buff<-st_transform(SST.buff,st_crs(SST))
SST.buff<-subset(SST.buff, select = c(id,geometry,hres))
system.time(SST.buff$meanSST<-exact_extract(SST, SST.buff, "mean")) # Extracting mean sea surface temp for each buffered polygon
SST.buff$geometry<-NULL # Turns sf object into dataframe
results2<-left_join(results2,SST.buff,by = c("id","hres"))
rm(SST.buff,SST) # Removing unneeded vars

# Adjacent chlorophyll a values, all formats (PUD, TUD, cell)
resultsPUD<-results[which(results$source=="PUD"),]
resultsTUD<-results[which(results$source=="TUD"),]
rs<-list.files("./Data/Chlorophyll", full.names = TRUE) # List of all Chlorophyll a monthly rasters, 2005 - 2019
rs.PUD<-rs[1:83] # Subset corresponding to 2005 - 2011 # Split up due to long processing time for dealing with 2005 - 2017 raster stack
rs.TUD<-rs[84:156] # Subset corresponding to 2012 - 2017
rs.cell<-rs[169:180] # Subset corresponding to 2019
s.PUD<-stack(rs.PUD) # Raster stacks
s.TUD<-stack(rs.TUD)
s.cell<-stack(rs.cell)
Ca.buff8m<-st_buffer(resultsPUD,dist=12784) # Creating a (8 mile) buffered shapefile of hexagons 
Ca.buff8m<-st_transform(Ca.buff8m,crs=crs(s.PUD))
Ca.buff8m<-subset(Ca.buff8m, select = c(id,geometry,hres))
system.time(Ca.buff8m$meanPUD<-rowMeans(exact_extract(s.PUD, Ca.buff8m, "mean"))) # Extracting mean precip for each buffered polygon, 2005 - 2011 
system.time(Ca.buff8m$meanTUD<-rowMeans(exact_extract(s.TUD, Ca.buff8m, "mean"))) # 2012 - 2017
system.time(Ca.buff8m$meancell<-rowMeans(exact_extract(s.cell, Ca.buff8m, "mean"))) # 2019
Ca.buff8m$meanPUD<-(Ca.buff8m$meanTUD+Ca.buff8m$meanPUD)/2 # PUD mean over 2005 - 2017 
Ca.buff8m$geometry<-NULL # Turns sf object into dataframe
resultsPUD<-Ca.buff8m %>% 
  dplyr::select(id,meanPUD,hres) %>% 
  left_join(resultsPUD,.,by = c("id","hres"))
resultsTUD<-Ca.buff8m %>% 
  dplyr::select(id,meanTUD,hres) %>% 
  left_join(resultsTUD,.,by = c("id","hres"))
results2<-Ca.buff8m %>% 
  dplyr::select(id,meancell,hres) %>% 
  left_join(results2,.,by = c("id","hres"))
colnames(results2)[colnames(results2) == 'meancell'] <- 'ChA'
colnames(resultsPUD)[colnames(resultsPUD) == 'meanPUD'] <- 'ChA'
colnames(resultsTUD)[colnames(resultsTUD) == 'meanTUD'] <- 'ChA'
rm(Ca.buff8m,s.cell,s.PUD,s.TUD,rs,rs.cell,rs.PUD,rs.TUD) # Removing unneeded vars

results2<-results2 %>% relocate(geometry, .after = last_col()) # Fixing for row binding
st_geometry(results2)<-"geometry" # Fixing for row binding
results<-rbind(results2,resultsTUD,resultsPUD)

results %>% 
  group_by(hres,source) %>% 
  summarise(hres_sum = sum(ud, na.rm = TRUE))

results$ud<-round(results$ud)

rm(results2,resultsPUD,resultsTUD)

## Additional data processing
#results$ud[is.na(results$ud)]<-0 # Replacing NAs in cell data with zero. Comment out and use next line if we want to drop them
#results<-results[complete.cases(results),] # Doesn't work with sf objects
results<-results[!sf::st_is_empty(results), ] %>% na.omit() # https://stackoverflow.com/questions/52173746/return-complete-cases-of-sf-object-in-r drops a few observations where the precipitation and air temp rasters returned NAs due to inadequate coverage, results in different num obs for cell data, as 2019 coverage isn't as complete as the mean values derived from the longer time series relevant for the twitter and flickr data
sapply(results, function(x) sum(is.na(x))) # Checking to see if there are any NAs
# rescaling population to thousands and distances to km, to deal with model convergence issues
results$sumpop<-results$sumpop/1000
results$rdist<-results$rdist/1000
results$wtlddist<-results$wtlddist/1000

# Filtering area of influence of amenities represented in Yourcoast database
results<-mutate_at(results, vars(BIKE_PATH,BOATING,BT_FACIL_T,CAMPGROUND,DSABLDACSS,FEE,FISHING,PARKING,PTH_BEACH,RESTROOMS,STRS_BEACH), list(~ ifelse(YCdist>1000, 0, .)))

# ESI crosswalk
results$RockyShore<-results$x1a + results$x2a + results$x6a +results$x8a
results$SandyBeach<-results$x3a + results$x3b + results$x4 + results$x5
results$Marshes<-results$x9a + results$x9b + results$x10a
results$Armored<-results$x1b + results$x6b + results$x8b + results$x8c

# County 
County<-st_read("./Data/CA_Counties_TIGER2016.gpkg")
County<-st_transform(County,st_crs(results))
County<-County %>% dplyr::select(NAME)
results2<-st_centroid(results) # Identifying county using centroid of observation polygon (issue for small fraction of polygons that cross county borders)
results2<-st_join(results2,County)
results2$geometry<-NULL # Turns sf object into dataframe
results2<-results2 %>% 
  dplyr::select(id,NAME,hres) %>% 
  unique()
sapply(results2, function(x) sum(is.na(x))) # Checking to see if there are any NAs 
results<-left_join(results,results2,by = c("id","hres"))
rm(results2,County)

## Write to disk
results %>% 
  st_drop_geometry() %>% 
  write.csv("lresults.csv",row.names = FALSE)

st_write(results,"lresults.gpkg")

#unlink("./Data", recursive = TRUE) # Delete data directory

# ## SFEI SF Bay shoreline inventory 
# # Note: Only captures locations in SF Bay Shoreline Inventory that overlay with NOAA ESI, which discards roughly 50% of data from the inventory
# SF<-st_read("Data/SF_Bay_Shoreline_Inventory.gpkg")
# SF<-st_transform(SF,st_crs(df))
# SF<-SF[,c("Class","geom")]
# # Setting up a temporary dataframe w/indicator variables for classes to join back to main dataframe
# df1<-st_join(df,SF) # Spatial join
# df1$Class<-as.character(df1$Class) 
# df1<-unique(df1) # Create a dataframe of only unique values (combinations of classes) within each hexagon
# df1$berm<-ifelse(df1$Class=="Berm",1,0)
# df1$channel<-ifelse(df1$Class=="Channel or Opening",1,0)
# df1$sps<-ifelse(df1$Class=="Shoreline Protection Structure",1,0)
# df1$embank<-ifelse(df1$Class=="Embankment",1,0)
# df1$levee<-ifelse(df1$Class=="Engineered Levee",1,0)
# df1$floodwall<-ifelse(df1$Class=="Floodwall",1,0)
# df1$naturalshore<-ifelse(df1$Class=="Natural Shoreline",1,0)
# df1$trans<-ifelse(df1$Class=="Transportation Structure",1,0)
# df1$watercontrol<-ifelse(df1$Class=="Water Control Structure",1,0)
# df1$wetland<-ifelse(df1$Class=="Wetland",1,0)
# df1<-subset(df1, select = c("id","berm","channel","sps","embank","levee","floodwall","naturalshore","trans","watercontrol","wetland"))
# df1$geometry<-NULL
# df1[is.na(df1)]<-0 # Replace NAs with zeros
# df1<-aggregate(. ~ id,df1,max) # Max value of each indicator variable, by id. Generates binary indicator even when class present > 1 times.
# # Rejoinging aggregated data to hexagons
# df<-merge(df,df1,"id")
# #st_write(df1,dsn="validation.gpkg",layer="df1")
# # Removing unneeded vars
# rm(df1, SF)
# # Indicator for membership in SF Bay
# df$SF<-df$berm+df$channel+df$sps+df$embank+df$levee+df$floodwall+df$naturalshore+df$trans+df$watercontrol+df$wetland
# df$SF<-ifelse(df$SF>0,1,0)


