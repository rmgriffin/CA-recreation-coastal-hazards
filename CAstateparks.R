#### Use this to start every program.  This clears out previous information from memory
rm(list=ls())
options(scipen=999) # Bias against scientific notation

# Packages
PKG <- c("rgdal", "maptools", "gridExtra", "sf", "rgeos", "reticulate", "raster", "ggplot2", "reshape", "foreign", "dplyr", "stringr") 

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}
rm(PKG,p)
renv::snapshot()

#### Parks vector
parks<-shapefile("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/caStateParks_20190225c/calStateParks_20190225c_polygon_ta.shp")

#### Transforming polygon shapefile to: 1) group polygons with same name into a single feature; 2) have consistent naming with surveyed data   
### Self intersection issue - resolving https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec?rq=1
parks2<-gBuffer(parks, byid=TRUE, width=0)
### Dealing with naming issues
## Unique levels for checking against csv for consistency
sort(unique(parks2$Name))
## Fixing specific cases (did most of this by manually editing parks survey CSV names)
# Two names for Ano Nuevo
parks2[parks2$Name=="AÃ±o Nuevo Coast NP"]<-"Ano Nuevo SP"
### Combine features that represent the same park https://gis.stackexchange.com/questions/63577/joining-polygons-in-r/273515
parks3<-unionSpatialPolygons(parks2,parks2$Name)
### Does not preserve IDs as part of the attribute table http://lojze.lugos.si/~darja/software/r/library/maptools/html/unionSpatialPolygons.html
Name<-as.data.frame(getSpPPolygonsIDSlots(parks3))
## Renaming col header
colnames(Name)<-c("Name")
## Appending Names to attribute table https://gis.stackexchange.com/questions/279065/merging-two-spatial-objects-in-r-spcbind-gives-error-row-names-not-identical
parks3$Name<-Name 

#### Import parks surveyed visitation data
data<-read.csv("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/Reports/Tabula_extract.csv")

#### Merge surveyed data to shapefile
parks4<-merge(parks3, data, by = "Name")
### View attribute table
parks4@data
### Dropping parks with NA values https://gis.stackexchange.com/questions/53265/removing-rows-in-shapefile-in-r
parks4<-parks4[!is.na(parks4@data$X2012),] # Uses 2012 column, but any of the visitation columns would work

#### Indicator variable for coast adjacent, defined as intersecting a 2km buffer of the ESI 
### Import buffer
coast<-shapefile("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Outer/Twitter/ca_allcoast_2kmbuffer_4tweets_wgs84/ca_allcoast_2kmbuffer_4tweets_wgs84_ta.shp") # Predefined an indicator variable in the attribute table
### Spatial join of shapefiles http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html 
a<-as.data.frame(over(parks4,coast))
### NA's to 0
a[is.na(a),]<-0
parks4@data$Ind<-a$Indicator # https://stackoverflow.com/questions/23665328/error-saving-new-shape-file-after-using-over-function-r

#### Export shapefile
#shapefile(parks4, "C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/caStateParks_20190225c/calStateParks_20190225c_polygon_ta_grouped.shp", overwrite = TRUE)

#### Running 4 separate calls to python to generate annual flickr visitation numbers for 2012 - 2016 (Visitation is reported from July 1 to June 30 of the following year, therefore the relevant comparison requires us to manually assemble annual PUD for two years by adding up the relevant months)
### Use conda environment "InVEST 3.6.0" that is tested to work with invest 3.5.0 installation
#use_condaenv("InVEST 3.6.0")

### Call to python scripts
#py_run_file("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/InVEST/recmodel_client_12.py")
#py_run_file("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/InVEST/recmodel_client_13.py")
#py_run_file("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/InVEST/recmodel_client_14.py")
#py_run_file("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/InVEST/recmodel_client_15.py")
#py_run_file("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/InVEST/recmodel_client_16.py")

#### Reading dbfs of InVEST results
x12<-read.dbf("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/InVEST/pud_results_x12.dbf")
x13<-read.dbf("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/InVEST/x13/pud_results_calStateParks_20190225c_polygon_ta_grouped.dbf")
x14<-read.dbf("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/InVEST/pud_results_x14.dbf")
x15<-read.dbf("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/InVEST/x15/pud_results_calStateParks_20190225c_polygon_ta_grouped.dbf")
x16<-read.dbf("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/InVEST/x16/pud_results_calStateParks_20190225c_polygon_ta_grouped.dbf")

#### Creating vector sums of July to following June visitation across all parks
### 2012 - 2013 PUD
parks4@data$PUD1213<-rowSums(x12[,14:19])+rowSums(x13[,3:8])
### 2013 - 2014 PUD
parks4@data$PUD1314<-rowSums(x13[,9:14])+rowSums(x14[,8:13])
### 2014 - 2015 PUD
parks4@data$PUD1415<-rowSums(x14[,14:19])+rowSums(x15[,3:8])
### 2015 - 2016 PUD
parks4@data$PUD1516<-rowSums(x15[,9:14])+rowSums(x16[,3:8])

#### Calculating area of each park
parks4@data$sqkm<-area(parks4)/1000000

#### Correlation analysis
### Create dataframe
df<-as.data.frame(parks4) 
### All years combined variables
df$PUDall<-df$PUD1213+df$PUD1314+df$PUD1415+df$PUD1516
df$Xall<-df$X2012+df$X2013+df$X2014+df$X2015
### Create inverse hyperbolic sine transformations of all visitation variables (use IHS to deal with lots of zeros that we don't want to throw away http://marcfbellemare.com/wordpress/12856#more-12856
## Define inverse hyperbolic sine function
ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}
## Transformations
df$ihsx1213<-ihs(df$X2012)
df$ihsx1314<-ihs(df$X2013)
df$ihsx1415<-ihs(df$X2014)
df$ihsx1516<-ihs(df$X2015)
df$ihsPUD1213<-ihs(df$PUD1213)
df$ihsPUD1314<-ihs(df$PUD1314)
df$ihsPUD1415<-ihs(df$PUD1415)
df$ihsPUD1516<-ihs(df$PUD1516)
df$ihsxall<-ihs(df$Xall)
df$ihsPUDall<-ihs(df$PUDall)

## 2km coast adjacent indicator as factor
df$Coast<-as.factor(df$Ind)

### Plot
## Basic plot of all data
ggplot(df, aes(x=ihsxall, y=ihsPUDall, color=Coast)) + geom_point() + 
  labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
  theme_classic()
## By year
ggplot(df, aes(x=ihsx1213, y=ihsPUD1213), color = "#f0f9e8") + geom_point() + 
  labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
  theme_classic()+
  geom_point(data=df, aes(x=ihsx1314, y=ihsPUD1314), color = "#bae4bc") +
  geom_point(data=df, aes(x=ihsx1415, y=ihsPUD1415), color = "#7bccc4") +
  geom_point(data=df, aes(x=ihsx1516, y=ihsPUD1516), color = "#2b8cbe")
## Only coast adjacent
ggplot(df %>% filter(Coast == 1), aes(x=ihsxall, y=ihsPUDall)) + geom_point() + 
  labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
  theme_classic()
## Only comparisons where CDPR estimates are greater than zero
ggplot(df %>% filter(ihsxall > 0), aes(x=ihsxall, y=ihsPUDall, color=Coast)) + geom_point() + 
  labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
  theme_classic()

### R2
## OLS of all observations
summary(lm(ihsPUDall ~ ihsxall, data = df))
nobs(lm(ihsPUDall ~ ihsxall, data = df))
## OLS of observations where CDPR estimates are greater than zero
summary(lm(ihsPUDall ~ ihsxall, data = df %>% filter(ihsxall > 0)))
nobs(lm(ihsPUDall ~ ihsxall, data = df %>% filter(ihsxall > 0)))
## OLS of observations where CDPR estimates are greater than zero and only coastal parks
summary(lm(ihsPUDall ~ ihsxall, data = df %>% filter(ihsxall > 0 & Coast == 1)))
nobs(lm(ihsPUDall ~ ihsxall, data = df %>% filter(ihsxall > 0 & Coast == 1)))

#### Fee-based visitation is more accurate in this dataset
### Import parks surveyed visitation data that has categories for fee-based and free
data_all<-read.csv("C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/Reports/Tabula_extract_all.csv")
### Merge surveyed data to PUD shapefile
parks5<-merge(parks3, data_all, by = "Name")
### View attribute table
parks5@data
### Dropping parks with NA values https://gis.stackexchange.com/questions/53265/removing-rows-in-shapefile-in-r
parks5<-parks5[!is.na(parks5@data$Pay15),] # Uses pay15 column, but any of the visitation columns would work

### Creating vector sums of July to following June visitation across all parks
## 2012 - 2013 PUD
parks5@data$PUD1213<-rowSums(x12[,14:19])+rowSums(x13[,3:8])
## 2013 - 2014 PUD
parks5@data$PUD1314<-rowSums(x13[,9:14])+rowSums(x14[,8:13])
## 2014 - 2015 PUD
parks5@data$PUD1415<-rowSums(x14[,14:19])+rowSums(x15[,3:8])
## 2015 - 2016 PUD
parks5@data$PUD1516<-rowSums(x15[,9:14])+rowSums(x16[,3:8])

### Extract dataframe
df_all<-as.data.frame(parks5)

### Fixing formatting in dataframe
## Factor to numeric
# Replacing all hyphens
df_all<-df_all %>% # https://stackoverflow.com/questions/29271549/replace-all-occurrences-of-a-string-in-a-data-frame
  mutate_all(funs(str_replace(., "-", "")))
# Converting to numeric https://stackoverflow.com/questions/2288485/how-to-convert-a-data-frame-column-to-numeric-type
df_all[,2:21] <- sapply(df_all[,2:21], as.numeric)
### Removing all observations with non-positive numbers of paying
df_all<-subset(df_all, Pay15>0)

### Preparing variables for plotting
## Aggregate (paid) CDPR visitation and PUD across 2012 - 2016, transformed via inverse hyperbolic sine
df_all$ihsx1216<-ihs(df_all$Pay12+df_all$Pay13+df_all$Pay14+df_all$Pay15)
df_all$ihsPUD1216<-ihs(df_all$PUD1213+df_all$PUD1314+df_all$PUD1415+df_all$PUD1516)
df_all$ihsPUD1516<-ihs(df_all$PUD1516)
df_all$ihsx1516<-ihs(df_all$Pay15)

### Plotting
ggplot(df_all, aes(x=ihsx1216, y=ihsPUD1216)) + geom_point() + 
  labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
  theme_classic()

### R2
## OLS of all paid fee observations 
summary(lm(ihsPUD1216 ~ ihsx1216, data = df_all))
nobs(lm(ihsPUD1216 ~ ihsx1216, data = df_all))
## OLS of 2015-2016 paid fee observations 
summary(lm(ihsPUD1516 ~ ihsx1516, data = df_all))
nobs(lm(ihsPUD1516 ~ ihsx1516, data = df_all))


### Remove unneeded objects
rm(a, coast, Name, parks, parks2, parks3, data, data_all, x12, x13, x14, x15,x16)

# Export data
#write.csv(df, "C:/Users/XPSXIII/Documents/Stanford/California/Recreation/Data/Validation/CA_state_park/datafromR.csv")

