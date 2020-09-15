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

# # Library for negative binomial regression
# library(MASS)
# # Library for rootogram for negative binomial regression
# library(countreg)
# # Library for visualizing regression results (also need broom/ggstance/ package for plots) https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
# library(jtools)
# library(broom)
# library(ggstance)
# #Plots
# library(ggplot2)
# # Library to easily delimit columns
# library(tidyr)
# # Library for hurdle models
# library(pscl)
# # Library for correlograms
# library(corrplot)

### Downloading data
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

### Data processing function to run across resolutions
data.processing<-function(f,t){

  ### Dependent variables
  Flickr<-st_read(f)
  # Convert PUD_YR_INT to PUD by multiplying by # (covering the time horizon 2005 - 2017). SWITCH to relevant input (twitter, flickr, etc.)
  Flickr$PUD<-Flickr$PUD_YR_AVG*13
  # Add id
  Flickr$id<-seq.int(nrow(Flickr))
  Twitter<-st_read(t)
  Flickr$TUD<-Twitter$PUD_YR_AVG*6
  Depvar<-Flickr[,c("PUD","TUD","id")]
  
  ### Independent variables
  ## ESI
  ESI<-st_read("Data/ESIL_CA.gpkg")
  ESI<-st_transform(ESI, st_crs(Depvar))
  ESI<-ESI[,c("ESI","geom")]
  # Spatial join of ESI and Depvar
  df<-st_join(Depvar,ESI) # Spatial join
  df$ESI<-as.character(df$ESI)
  # Create a dataframe of only unique values (combinations of classes) within each hexagon
  df<-unique(df)
  df<-separate(df,ESI,c("I1","I2","I3"),"/",convert = TRUE) 
  # Create indicators for all classes
  df$x1a<-ifelse(df$I1 == "1A",1, ifelse(df$I2 == "1A", 1, ifelse(df$I3 == "1A",1,0)))
  df$x1b<-ifelse(df$I1 == "1B",1, ifelse(df$I2 == "1B", 1, ifelse(df$I3 == "1B",1,0)))
  df$x2a<-ifelse(df$I1 == "2A",1, ifelse(df$I2 == "2A", 1, ifelse(df$I3 == "2A",1,0)))
  df$x3a<-ifelse(df$I1 == "3A",1, ifelse(df$I2 == "3A", 1, ifelse(df$I3 == "3A",1,0)))
  df$x3b<-ifelse(df$I1 == "3B",1, ifelse(df$I2 == "3B", 1, ifelse(df$I3 == "3B",1,0)))
  df$x4<-ifelse(df$I1 == "4",1, ifelse(df$I2 == "4", 1, ifelse(df$I3 == "4",1,0)))
  df$x5<-ifelse(df$I1 == "5",1, ifelse(df$I2 == "5", 1, ifelse(df$I3 == "5",1,0)))
  df$x6a<-ifelse(df$I1 == "6A",1, ifelse(df$I2 == "6A", 1, ifelse(df$I3 == "6A",1,0)))
  df$x7<-ifelse(df$I1 == "7",1, ifelse(df$I2 == "7", 1, ifelse(df$I3 == "7",1,0)))
  df$x6b<-ifelse(df$I1 == "6B",1, ifelse(df$I2 == "6B", 1, ifelse(df$I3 == "6B",1,0)))
  df$x8a<-ifelse(df$I1 == "8A",1, ifelse(df$I2 == "8A", 1, ifelse(df$I3 == "8A",1,0)))
  df$x8b<-ifelse(df$I1 == "8B",1, ifelse(df$I2 == "8B", 1, ifelse(df$I3 == "8B",1,0)))
  df$x8c<-ifelse(df$I1 == "8C",1, ifelse(df$I2 == "8C", 1, ifelse(df$I3 == "8C",1,0)))
  df$x9a<-ifelse(df$I1 == "9A",1, ifelse(df$I2 == "9A", 1, ifelse(df$I3 == "9A",1,0)))
  df$x9b<-ifelse(df$I1 == "9B",1, ifelse(df$I2 == "9B", 1, ifelse(df$I3 == "9B",1,0)))
  df$x10a<-ifelse(df$I1 == "10A",1, ifelse(df$I2 == "10A", 1, ifelse(df$I3 == "10A",1,0)))
  df$xU<-ifelse(df$I1 == "U",1, ifelse(df$I2 == "U", 1, ifelse(df$I3 == "U",1,0)))
  # Replace NAs with zeros
  df[is.na(df)]<-0
  # Aggregating data by factor (observation is a hexagon, unique by id). "Max" ensures we get a 1, even when there are multiple 1's for the same class per hexagon
  df<-subset(df, select = c("id","x1a","x1b","x2a","x3a","x3b","x4","x5","x6a","x6b","x7","x8a","x8b","x8c","x9a","x9b","x10a","xU"))
  df$geom<-NULL
  df<-aggregate(. ~ id,df,max)
  # Rejoinging aggregated data to hexagons
  df<-merge(Depvar,df,"id")
  # Lots of observations that do not overlap shoreline layer
  df$types<-df$x1a+df$x1b+df$x2a+df$x3a+df$x3b+df$x4+df$x5+df$x6a+df$x6b+df$x7+df$x8a+df$x8b+df$x8c+df$x9a+df$x9b+df$x10a+df$xU
  table(df$types)
  df<-df[which(df$types!=0),]
  #st_write(df,dsn="validation.gpkg",layer="df")
  # Removing unneeded vars
  rm(Depvar, ESI)
  
  ## Adjacent population derived from EPA population raster
  Pop<-raster("Data/dasymetric_us_20160208.tif")
  # Creating a (8 mile) buffered shapefile of hexagons
  Pop.buff8m<-st_buffer(df,dist=12784)
  Pop.buff8m<-st_transform(Pop.buff8m,st_crs(Pop))
  # Extracting sum of population raster for each buffered polygon 
  system.time(Pop.buff8m$sumpop<-exact_extract(Pop, Pop.buff8m, "sum"))
  # Merging to dataframe
  Pop.buff8m<-Pop.buff8m[,c("id","sumpop")]
  Pop.buff8m$geometry<-NULL # Turns sf object into dataframe
  df<-merge(df,Pop.buff8m,"id")
  # Removing unneeded vars
  rm(Pop,Pop.buff8m)
  
  ## Road network distance
  # Road network line shapefile
  Rd<-st_read("Data/Roads_2015.gpkg")
  Rd<-st_transform(Rd, st_crs(df))
  # Nearest distance, following https://stackoverflow.com/questions/53854803/calculate-minimum-distance-between-multiple-polygons-with-r
  system.time(index<-st_nearest_feature(df, Rd)) # Creates an ordered list (index) of closest elements from Rd shapefile to each feature in "df"
  Rd<-Rd %>% slice(index) # Subsets the Rd shapefile by creating a reordered version based on the element ordering of "index"
  df$rdist<-as.vector(st_distance(df, Rd, by_element = TRUE)) # Calculates distance between two sf objects, element-by-element. as.vector removes [m] units
  rm(Rd)
  
  ## Adding "Yourcoast" data from California Coastal Commission and postprocessing
  YC<-st_read("Data/coastal_access_locations.gpkg")
  YC<-st_transform(YC,st_crs(df))
  # "st_join, nearest" following https://stackoverflow.com/questions/53854803/calculate-minimum-distance-between-multiple-polygons-with-r
  index<-st_nearest_feature(df, YC) # Creates an ordered list (index) of closest elements from YC shapefile to each feature in "df"
  YC<-YC %>% slice(index) # Subsets the YC shapefile by creating a reordered version based on the element ordering of "index"
  df$YCdist<-as.vector(st_distance(df, YC, by_element = TRUE)) # Calc dist between two sf objects, element-by-element; as.vector removes [m] units
  df<-cbind(df,YC) # Column binding
  rm(YC)
  # Dropping variables we are not going to use in the analysis (see google doc)
  df<-subset(df, select = -c(BLFTP_PRK, BLFTP_TRLS, BLUFF, DOG_FRIEND, DUNES, EZ4STROLLE, PCNC_AREA, RKY_SHORE, SNDY_BEACH, TIDEPOOL, VISTOR_CTR, VOLLEYBALL, WLDLFE_VWG, COUNTY, CountyNum, DISTRICT, Descriptio, GEOGR_AREA, ID, LATITUDE, LONGITUDE, LIST_ORDER, LocationMo, NameMobile, PHONE_NMBR, Photo_1, Photo_2, Photo_3, Photo_4, Bch_whlchr, geom))
  # all "no" to zero, and all "yes" to 1 for indicator variables, Note, there are a range of values here {?, No, Yes, Yes?, yes, no, NO} and we recode these as {0, 0, 1, 1, 1, 0, 0} where 1 == yes and 0 == no.
  df<-rapply(df, as.character, classes="factor", how="replace") # Factors as character https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
  df <- df %>% 
    mutate_if(is.character,funs(str_replace(., c("\\?|No|NO|no"), "0"))) # http://www2.stat.duke.edu/~cr173/Sta523_Fa15/regex.html
  df <- df %>% 
    mutate_if(is.character,funs(str_replace(., c("Yes|yes|Yes\\?"), "1")))
  # Boating facilities (is different from "BOATING", though mostly overlap)
  table(df$BT_FACIL_T) # An array of facilities, assign 1 if something, 0 if nothing
  df$BT_FACIL_T<-ifelse(is.na(df$BT_FACIL_T),0,ifelse(df$BT_FACIL_T==0,0,1))
  # Bike path seems to have a coding error (10 instead of 1)
  df$BIKE_PATH<-ifelse(df$BIKE_PATH==10,1,df$BIKE_PATH)
  df<-rapply(df, as.numeric, classes="character", how="replace") # Character as numeric
  # Any NAs? 
  sapply(df, function(x) sum(is.na(x)))
  # Replace NAs with zero
  df[is.na(df)]<-0
  
  ## Water trail (existing sites as of 2016)
  WT<-st_read("Data/WaterTrailSites_July2016.gpkg")
  WT<-WT[,c("WaterTrail","geom")]
  WT<-st_transform(WT,st_crs(df))
  # Spatial join
  df<-st_join(df,WT)
  df$WaterTrail<-ifelse(is.na(df$WaterTrail),0,df$WaterTrail)
  rm(WT)
  
  ## Bay trail
  BT<-st_read("Data/Mar_2018_BayTrail.gpkg")
  # When were certain sections built? Allow querying
  BT<-BT[which(BT$STATUS=="Existing"),] # Only currently existing projects
  BT$Year_Compl<-as.character(BT$Year_Compl)
  BT$Year_Compl<-ifelse(is.na(BT$Year_Compl),1989,
                        ifelse(BT$Year_Compl=="pre-1989",1989,BT$Year_Compl)) # Assuming all NAs are 1989 or prior
  BT$Year_Compl<-as.numeric(BT$Year_Compl)
  # Filter by year here
  #BT<-BT[which(BT$YearCompl<=2004),]
  # Indicator variable
  BT$BayTrail<-1
  BT<-BT[,c("BayTrail","geom")]
  # Spatial join
  df<-st_join(df,BT)
  df<-unique(df) # Two or more lines may exist within one hexagon - only need one
  df$BayTrail[is.na(df$BayTrail)]<-0 # Replacing NAs with zero
  rm(BT)
  
  ## Distance to nearest marsh
  DM<-st_read("Data/nlcd_wetlands_2016.gpkg")
  DM<-st_transform(DM, st_crs(df))
  # Nearest distance, following https://stackoverflow.com/questions/53854803/calculate-minimum-distance-between-multiple-polygons-with-r
  system.time(index<-st_nearest_feature(df, DM)) # Creates an ordered list (index) of closest elements from Rd shapefile to each feature in "df"
  DM<-DM %>% slice(index) # Subsets the Rd shapefile by creating a reordered version based on the element ordering of "index"
  df$wtlddist<-as.vector(st_distance(df, DM, by_element = TRUE)) # Calculates distance between two sf objects, element-by-element. as.vector removes [m] units
  rm(DM, index)
  
  ## Adjacent precipitation
  rs.f<-list.files("./Data/Prism Precip", full.names = TRUE)
  rs.t<-rs.f[8:13]
  s.f<-stack(rs.f)
  s.t<-stack(rs.t)
  # Creating a (8 mile) buffered shapefile of hexagons 
  Prec.buff8m<-st_buffer(df,dist=12784)
  Prec.buff8m<-st_transform(Prec.buff8m,crs=crs(s.f))
  Prec.buff8m<-subset(Prec.buff8m, select = c(id,geometry))
  # Extracting mean precip for each buffered polygon 
  system.time(Prec.buff8m$meanprecf<-rowMeans(exact_extract(s.f, Prec.buff8m, "mean")))
  system.time(Prec.buff8m$meanprect<-rowMeans(exact_extract(s.t, Prec.buff8m, "mean")))
  # Merging to dataframe
  Prec.buff8m<-Prec.buff8m[,c("id","meanprecf","meanprect")]
  Prec.buff8m$geometry<-NULL # Turns sf object into dataframe
  df<-merge(df,Prec.buff8m,"id")
  # Removing unneeded vars
  rm(s.f,s.t,Prec.buff8m,rs.f,rs.t)
  
  ## Adjacent air temperature
  rs.f<-list.files("./Data/Prism Mean Temp", full.names = TRUE)
  rs.t<-rs.f[8:13]
  s.f<-stack(rs.f)
  s.t<-stack(rs.t)
  # Creating a (8 mile) buffered shapefile of hexagons 
  AT.buff8m<-st_buffer(df,dist=12784)
  AT.buff8m<-st_transform(AT.buff8m,crs=crs(s.f))
  AT.buff8m<-subset(AT.buff8m, select = c(id,geometry))
  # Extracting mean air temp for each buffered polygon 
  system.time(AT.buff8m$meanatf<-rowMeans(exact_extract(s.f, AT.buff8m, "mean")))
  system.time(AT.buff8m$meanatt<-rowMeans(exact_extract(s.t, AT.buff8m, "mean")))
  # Merging to dataframe
  AT.buff8m<-AT.buff8m[,c("id","meanatf","meanatt")]
  AT.buff8m$geometry<-NULL # Turns sf object into dataframe
  df<-merge(df,AT.buff8m,"id")
  # Removing unneeded vars
  rm(AT.buff8m,s.f,s.t,rs.f,rs.t)
  
  ## Adjacent sea surface temperature
  SSTf<-raster("Data/flickr_GIOVANNI-g4.timeAvgMap.MODISA_L3m_SST_Monthly_4km_R2019_0_sst.20050101-20171231.125W_32N_116W_42N.tif")
  SSTt<-raster("Data/twitter_GIOVANNI-g4.timeAvgMap.MODISA_L3m_SST_Monthly_4km_R2019_0_sst.20120101-20171231.125W_32N_116W_42N.tif")
  # Creating a (15km) buffered shapefile of hexagons
  SST.buff<-st_buffer(df,dist=15000)
  SST.buff<-st_transform(SST.buff,crs=crs(SSTf))
  # Extracting mean sea surface temp for each buffered polygon
  system.time(SST.buff$meanSSTf<-exact_extract(SSTf, SST.buff, "mean"))
  system.time(SST.buff$meanSSTt<-exact_extract(SSTt, SST.buff, "mean"))
  # Merging to dataframe
  SST.buff<-SST.buff[,c("id","meanSSTf","meanSSTt")]
  SST.buff$geometry<-NULL # Turns sf object into dataframe
  df<-merge(df,SST.buff,"id")
  # Removing unneeded vars
  rm(SST.buff,SSTf,SSTt)
  
  ## Transferring to long format with flickr/twitter id (PUD vs TUD)
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

write.csv(results,"results.csv")

unlink("./Data", recursive = TRUE) # Delete data directory

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

## Exporting data for regression analysis in STATA
#write.csv(df, "C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/t_dataframe_CA_500m.csv")


### Start here


# ### Descriptive statistics and data exploration
# # Frequencies of amenity and access and other predictors
# dfSummary(YDM$amenity)
# dfSummary(YDM$access)
# dfSummary(YDM$Pop)
# dfSummary(YDM$NEAR_DIST)
# dfSummary(YDM$PUD)
# # CDF of PUD
# z<-ecdf(YDM$PUD)
# plot(z)
# # Histogram of PUD
# hist(YDM$PUD, breaks=100)
# # Number of zeros in PUD and percent zeros
# length(which(YDM$PUD==0))
# length(which(YDM$PUD==0))/length(YDM$PUD)*100
# # Frequency for all shoreline classes
# freq.ESI<-colSums(YDM[,c("x1a", "x1b", "x2a", "x3a", "x3b", "x4", "x5", 
#               "x6a", "x6b", "x8a", "x8b", "x8c", "x9a", "x9b", "x10a")])/3781*100
# # Drop xU class, as there are no observations
# YDM$xU<-NULL
# # Scatter plots of PUD vs indep vars
# scatter.smooth(YDM$Pop,YDM$PUD)
# scatter.smooth(YDM$NEAR_DIST,YDM$PUD)
# 
# 
# ### Regression analysis
# # ## Drop all obs that don't intersect with SFEI shoreline inventory (occurs because resample of SFEI shoreline inventory to raster yields no overlapping cells, even though line vector intersects visitation hexagon)
# # # New dataframe for regression as I don't want to ditch non-complete cases needed for scenario runs https://stackoverflow.com/questions/11254524/omit-rows-containing-specific-column-of-na
# # completeFun <- function(data, desiredCols) {
# #   completeVec <- complete.cases(data[, desiredCols])
# #   return(data[completeVec, ])
# # }
# # df<-completeFun(df,"SFEI.mode")
# 
# ## Regression for diagnostic of model type (poisson, nb, hurdle, etc.) 
# # Cross-correlation - need a model that has few correlated predictors - indicates highest correlation between/within amenities and access   
# corYDM<-cor(YDM[,c("x1a", "x1b", "x2a", "x3a", "x3b", "x4", "x5", 
#            "x6a", "x6b", "x8a", "x8b", "x8c", "x9a", "x9b", "x10a", "Pop", 
#            "NEAR_DIST", "BIKE_PATH", "BOATING", "CAMPGROUND", "DSABLDACSS", 
#            "FEE", "FISHING", "PARKING", "PTH_BEACH", "RESTROOMS", "STRS_BEACH", 
#            "access", "amenity")])
# corrplot(corYDM)
# # Including all covariates, with grouped amenities and access
# summary(fit1<-glm(PUD ~ Pop + NEAR_DIST + FEE + access + amenity + x1a + x1b + x2a + x3a + x3b + x4 + x5 + x6a + x6b + x8a + x8b + x8c + x9a + x9b + x10a, family = "poisson", data = YDM))
# # How many zeros does this predict, compared to the value observed in data? https://data.library.virginia.edu/getting-started-with-hurdle-models/
# mu<-predict(fit1,type="response")
# exp <- sum(dpois(x = 0, lambda = mu))
# round(exp)
# sum(YDM$PUD < 1) 
# rootogram(fit1)
# # Model severely underpredicts zeros at 1km hexagonal size, try zero inflated poisson
# summary(fit1<-zeroinfl(PUD ~ Pop + NEAR_DIST + FEE + access + amenity + x1a + x1b + x2a + x3a + x3b + x4 + x5 + x6a + x6b + x8a + x8b + x8c + x9a + x9b + x10a, data = YDM))
# # Problem perhaps with degrees of freedom https://stat.ethz.ch/pipermail/r-sig-ecology/2012-May/003013.html trying a smaller model
# YDM$RockyShore<-pmax(YDM$x1a,YDM$x2a,YDM$x6a,YDM$x8a)
# YDM$SandyBeach<-pmax(YDM$x3a,YDM$x3b,YDM$x4,YDM$x5)
# YDM$Marshes<-pmax(YDM$x9a,YDM$x9b,YDM$x10a)
# YDM$Armored<-pmax(YDM$x1b,YDM$x6b,YDM$x8b,YDM$x8c)
# summary(fit1<-glm(PUD ~ Pop + NEAR_DIST + FEE + access + amenity + RockyShore + SandyBeach + Marshes + Armored, family = "poisson", data = YDM))
# summary(fit1<-zeroinfl(PUD ~ Pop + NEAR_DIST + FEE + access + amenity + RockyShore + SandyBeach + Marshes + Armored, data = YDM))
# # Not the problem, perhaps we need to center and scale the continuous predictors - here we have one variable (Pop) that is several orders of magnitude larger than the other predictors https://stat.ethz.ch/pipermail/r-sig-ecology/2012-May/003013.html
# YDM$Pop_scale<-scale(YDM$Pop)
# summary(fit1<-glm(PUD ~ Pop_scale + NEAR_DIST + FEE + access + amenity + RockyShore + SandyBeach + Marshes + Armored, family = "poisson", data = YDM))
# summary(fit1<-zeroinfl(PUD ~ Pop_scale + NEAR_DIST + FEE + access + amenity + RockyShore + SandyBeach + Marshes + Armored, data = YDM))

# Trying a smaller model that uses the proposed crosswalk

# summary(fit1<-hurdle(PUD ~ Pop + NEAR_DIST + FEE + access + amenity + RockyShore + SandyBeach + Marshes + Armored | Pop + NEAR_DIST, data = YDM, dist = "poisson", zero.dist = "binomial"))
# 
# 
# 
# 
# # No coast type covariates
# summary(fit1<-glm(PUD ~ Pop + NEAR_DIST + FEE + access + amenity, family = "poisson", data = YDM))
# # Including coast type covariates
# summary(fit1<-glm(PUD ~ Pop + NEAR_DIST + FEE + access + amenity + x1a + x1b + x2a + x3a + x3b + x4 + x5 + x6a + x6b + x8a + x8b + x8c + x9a + x9b + x10a, family = "poisson", data = YDM))
# 
# 
# 
# 
# summary(fit2<-glm(PUD ~ D_Road + Pop.sum + wetland + othernatural, family = "poisson", data=df))
# summary(fit3<-glm(PUD ~ D_Road + Pop.sum + wetland + D_Wetland + othernatural, family = "poisson", data=df))
# summary(fit4<-glm(PUD ~ D_Road + Pop.sum + wetland + D_Wetland + othernatural + BayT, family = "poisson", data=df))
# summary(fit5<-glm(PUD ~ D_Road + Pop.sum + wetland + D_Wetland + beach + naturalshore + BayT, family = "poisson", data=df))
# 
# ## Visualizing regression results
# plot_summs(fit1,fit2,fit3, fit4, fit5, scale = TRUE, exp = TRUE)

#coefs = c("D_Road" = "Distance to nearest road", "Pop.sum" = "Adjacent population within 8km", "nnbf" = "NNBF shoreline type", "wetland" = "Wetland shoreline type", "othernatural" = "All NNBF types except wetland", "D_Wetland" = "Distance to wetland", "BayT"="Bay Trail present", "beach" = "Beach shoreline type", "naturalshore" = "All NNBF types except wetland and beach






# ## Model fit https://www.fromthebottomoftheheap.net/2016/06/07/rootograms/
# # Plotting observed versus fitted values
# plot(df$PUD_YR_AVG,df$predict, xlim = c(0,800), ylim = c(0,800))
# # Rootogram
# rootogram(fit1)
# # Alt model where we round PUD to integers and re-estimate (confirms that rootogram only makes sense for integer discrete model)
# df$PUD_YR_INT<-round(df$PUD_YR_AVG)
# summary(fit2<-glm.nb(PUD_YR_INT~D_Road+Pop.sum+seawall+beach+wetland+trans+BayT+WaterTrail, data=df))
# rootogram(fit2)
# ## Predict values
# df$predict2<-predict(fit2,df,type="response")
# plot(df$PUD_YR_AVG,df$predict, xlim = c(0,800), ylim = c(0,800))















