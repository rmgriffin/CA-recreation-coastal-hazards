## Use this to start every program.  This clears out previous information from memory
rm(list=ls())

PKG <- c("sf","tidyverse","raster","exactextractr")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}
rm(p,PKG)

# # Loading rgdal library (needed for shapefiles, various geoprocessing)
# library(rgdal)
# # Loading raster library (used to append dataframes to a shapefile)
# library(raster)
# # Loading plyr library (used for renaming columns in data frame: http://www.cookbook-r.COM/Manipulating_data/Renaming_columns_in_a_data_frame/)
# library(plyr)
# # Loading dbfs https://www.datacamp.COM/COMmunity/tutorials/importing-data-r-part-two
# library(foreign)
# # Faster raster manipulation https://stackoverflow.com/questions/43439758/function-to-return-values-velox-raster
# library(velox)
# # Library that contains features for conducting a spatial join
# library(sp)
# # Library to do column binding to a polygon attribute table
# library(maptools)
# # Libraries for summary statistics https://dabblingwithdata-wordpress-com.cdn.ampproject.org/v/s/dabblingwithdata.wordpress.com/2018/01/02/my-favourite-r-package-for-summarising-data/amp/?amp_js_v=0.1#referrer=https%3A%2F%2Fwww.google.com&amp_tf=From%20%251%24s&ampshare=https%3A%2F%2Fdabblingwithdata.wordpress.com%2F2018%2F01%2F02%2Fmy-favourite-r-package-for-summarising-data%2F
# library(psych) # "describe" and "describeBy"
# library(summarytools) # dfSummary
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

### Dependent variable
Depvar<-st_read("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/DepVar/Twitter/Years_12-17/Regularized/250m/twud250m_extract.shp")
Depvar$FID_1<-as.numeric(Depvar$FID_1)
# Convert PUD_YR_INT to PUD by multiplying by # (covering the time horizon 2005 - 2017). SWITCH to relevant input (twitter, flickr, etc.)
Depvar$PUD<-Depvar$PUD_YR_AVG*6

# ## Other resolutions
# # 1km flickr and twitter user days data
# shapefile("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/DepVar/Flickr/Years_05-17/Regularized/1km/pud1km_extract.shp")
# shapefile("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/DepVar/Twitter/Years_12-17/Regularized/1km/twud1km_extract.shp")
# # 500m flickr and twitter user days data
# shapefile("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/DepVar/Flickr/Years_05-17/Regularized/500m/pud500m_extract.shp")
# shapefile("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/DepVar/Twitter/Years_12-17/Regularized/500m/twud500m_extract.shp")
# # 250m flickr and twitter user days data
# shapefile("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/DepVar/Flickr/Years_05-17/Regularized/250m/pud250m_extract.shp")
# shapefile("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/DepVar/Twitter/Years_12-17/Regularized/250m/twud250m_extract.shp")

### Independent variables
## ESI
ESI<-st_read("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/IndepVar/ESI/Combine/ESIL_CA_ta_prep.shp")
# Histogram of lengths of coastline represented in the ESI layer
hist(ESI$LENGTH, breaks = 1000, xlim = c(0,1000))
# Spatial join of ESI and Depvar
# Convert shapefiles to SF objects for spatial join
ESI<-st_as_sf(ESI)
Depvar<-st_as_sf(Depvar)
df<-st_join(Depvar,ESI) # Spatial join
df$LENGTH<-NULL # Removing length variable from ESI
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
# Aggregating data by factor (observation is a hexagon, unique by FID_1). "Max" ensures we get a 1, even when there are multiple 1's for the same class per hexagon
df<-subset(df, select = c("FID_1","x1a","x1b","x2a","x3a","x3b","x4","x5","x6a","x6b","x8a","x8b","x8c","x9a","x9b","x10a","xU"))
df$geometry<-NULL
df<-aggregate(. ~ FID_1,df,max)
# Rejoinging aggregated data to hexagons
df<-merge(Depvar,df,"FID_1")

## Adjacent population derived from EPA population raster
Pop<-raster("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/IndepVar/Pop/Outer_CA_Pop_8mile_p.tif")
# Creating a (8 mile) buffered shapefile of hexagons
Pop.buff8m<-st_buffer(Depvar,dist=12784)
# Extracting sum of population raster for each buffered polygon 
system.time(Pop.buff8m$sumpop<-exact_extract(Pop, Pop.buff8m, "sum")) #1300 - 1500s at 250m dep var resolution
# Merging to dataframe
Pop.buff8m<-Pop.buff8m[,c("FID_1","sumpop")]
Pop.buff8m$geometry<-NULL # Turns sf object into dataframe
df<-merge(df,Pop.buff8m,"FID_1")
# Removing unneeded vars
rm(Pop,Pop.buff8m)

## Road network distance
# Road network line shapefile
Rd<-st_read("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/IndepVar/Roads/Rd_CA_simp.shp")
Rd<-st_transform(Rd, st_crs(df))
# Nearest distance, following https://stackoverflow.com/questions/53854803/calculate-minimum-distance-between-multiple-polygons-with-r
index<-st_nearest_feature(df, Rd) # Takes ~ 180s @ 250m, creates an ordered list (index) of closest elements from Rd shapefile to each feature in "df"
Rd<-Rd %>% slice(index) # Subsets the Rd shapefile by creating a reordered version based on the element ordering of "index"
df$rdist<-as.vector(st_distance(df, Rd, by_element = TRUE)) # Calculates distance between two sf objects, element-by-element. as.vector removes [m] units
rm(Rd)

## Adjacent sea surface temperature (switch for twitter vs flickr analysis)
SST<-raster("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/IndepVar/WaterTemp/twitter_GIOVANNI-g4.timeAvgMap.MODISA_L3m_SST_Monthly_4km_R2019_0_sst.20120101-20171231.125W_32N_116W_42N.tif")
SST.buff4km<-st_buffer(Depvar,dist=4000)
SST.buff4km<-st_transform(SST.buff4km,crs=crs(SST))
system.time(SST.buff4km$meanSST<-exact_extract(SST, SST.buff4km, "mean")) # 23s 
# Merging to dataframe
SST.buff4km<-SST.buff4km[,c("FID_1","meanSST")]
SST.buff4km$geometry<-NULL # Turns sf object into dataframe
df<-merge(df,SST.buff4km,"FID_1")
# Removing unneeded vars
rm(SST,SST.buff4km)
hist(df$meanSST)

## Adjacent precipitation
Prec<-raster("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/IndepVar/Precip/PRISM_ppt_30yr_normal_800mM2_annual_asc.asc")
# Creating a (8 mile) buffered shapefile of hexagons 
Prec.buff8m<-st_buffer(Depvar,dist=12784)
Prec.buff8m<-st_transform(Prec.buff8m,crs=crs(Prec))
# Extracting mean precip for each buffered polygon 
system.time(Prec.buff8m$meanprec<-exact_extract(Prec, Prec.buff8m, "mean")) # 40s
# Merging to dataframe
Prec.buff8m<-Prec.buff8m[,c("FID_1","meanprec")]
Prec.buff8m$geometry<-NULL # Turns sf object into dataframe
df<-merge(df,Prec.buff8m,"FID_1")
# Removing unneeded vars
rm(Prec,Prec.buff8m)
hist(df$meanprec)

## Adjacent air temperature
AT<-raster("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/IndepVar/AirTemp/PRISM_tmean_30yr_normal_800mM2_annual_asc.asc")
# Creating a (8 mile) buffered shapefile of hexagons 
AT.buff8m<-st_buffer(Depvar,dist=12784)
AT.buff8m<-st_transform(AT.buff8m,crs=crs(AT))
# Extracting mean precip for each buffered polygon 
system.time(AT.buff8m$meanairt<-exact_extract(AT, AT.buff8m, "mean")) # 37s
# Merging to dataframe
AT.buff8m<-AT.buff8m[,c("FID_1","meanairt")]
AT.buff8m$geometry<-NULL # Turns sf object into dataframe
df<-merge(df,AT.buff8m,"FID_1")
# Removing unneeded vars
rm(AT,AT.buff8m)
hist(df$meanairt)

## Adding "Yourcoast" data from California Coastal Commission and postprocessing
YC<-st_read("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/IndepVar/coastal_access_locations/coastal_access_locations.shp")
YC<-st_transform(YC,st_crs(df))
# "st_join, nearest" following https://stackoverflow.com/questions/53854803/calculate-minimum-distance-between-multiple-polygons-with-r
index<-st_nearest_feature(df, YC) # Creates an ordered list (index) of closest elements from YC shapefile to each feature in "df"
YC<-YC %>% slice(index) # Subsets the YC shapefile by creating a reordered version based on the element ordering of "index"
df$YCdist<-as.vector(st_distance(df, YC, by_element = TRUE)) # Calc dist between two sf objects, element-by-element; as.vector removes [m] units
df<-cbind(df,YC) # Column binding
rm(YC)
df$geometry.1<-NULL
# Dropping variables we are not going to use in the analysis (see google doc)
df<-subset(df, select = -c(BLFTP_PRK, BLFTP_TRLS, BLUFF, DOG_FRIEND, DUNES, EZ4STROLLE, PCNC_AREA, RKY_SHORE, SNDY_BEACH, TIDEPOOL, VISTOR_CTR, VOLLEYBALL, WLDLFE_VWG, COUNTY, CountyNum, DISTRICT, Descriptio, GEOGR_AREA, ID, LATITUDE, LONGITUDE, LIST_ORDER, LocationMo, NameMobile, PHONE_NMBR, Photo_1, Photo_2, Photo_3, Photo_4, Bch_whlchr))
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















