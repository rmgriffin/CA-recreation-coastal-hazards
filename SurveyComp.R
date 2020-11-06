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

### CA State Parks

## Parks vector (annual visitation 2012 - 2016, with annual estimates running from 7/1 to 6/30 of the following year)
parks<-st_read("./Data/calStateParks_20190225c_polygon_ta.gpkg")
parks<-subset(parks, select = c(Name,geom))

## Transforming polygon shapefile to: 1) group polygons with same name into a single feature; 2) have consistent naming with surveyed data   
# Unique levels for checking against csv for consistency
sort(unique(parks$Name))
# Fixing specific cases (did most of this by manually editing parks survey CSV names)
parks[parks$Name=="AÃ±o Nuevo Coast NP"]<-"Ano Nuevo SP" # Two names for Ano Nuevo
# Combine features that represent the same park
parks<-parks %>% group_by(Name) %>%
  summarise(geometry = sf::st_combine(geom)) %>%
  ungroup()

## Import parks surveyed visitation data
data<-read.csv("./Data/Tabula_extract.csv")

## Merge surveyed data to shapefile
parks<-merge(parks, data, by = "Name")

## Indicator variable for "coast-adjacent," defined as intersecting a 1km buffer of ESI
PUD<-st_read("./Data/PUD_2005-2017_1000m.gpkg")
PUD$coast<-1
PUD<-subset(PUD, select = c(coast,geom))
PUD<-st_transform(PUD, st_crs(parks))
parks<-st_join(parks,PUD)
parks<-unique(parks)
parks[is.na(parks)]<-0

## Reading dbfs of InVEST results
x12<-read.dbf("./Data/pud_results_x12.dbf")
x13<-read.dbf("./Data/x13_pud_results_calStateParks_20190225c_polygon_ta_grouped.dbf")
x14<-read.dbf("./Data/pud_results_x14.dbf")
x15<-read.dbf("./Data/x15_pud_results_calStateParks_20190225c_polygon_ta_grouped.dbf")
x16<-read.dbf("./Data/x16_pud_results_calStateParks_20190225c_polygon_ta_grouped.dbf")

## Creating vector sums of July to following June visitation across all parks
# 2012 - 2013 PUD
parks$PUD1213<-rowSums(x12[,14:19])+rowSums(x13[,3:8])
# 2013 - 2014 PUD
parks$PUD1314<-rowSums(x13[,9:14])+rowSums(x14[,8:13])
# 2014 - 2015 PUD
parks$PUD1415<-rowSums(x14[,14:19])+rowSums(x15[,3:8])
# 2015 - 2016 PUD
parks$PUD1516<-rowSums(x15[,9:14])+rowSums(x16[,3:8])
rm(x12,x13,x14,x15,x16,data)

## Calculating area of each park
parks$sqkm<-st_area(parks)/1000000

## Correlation analysis
parks$PUDall<-parks$PUD1213+parks$PUD1314+parks$PUD1415+parks$PUD1516
parks$Xall<-parks$X2012+parks$X2013+parks$X2014+parks$X2015
# Create inverse hyperbolic sine transformations of all visitation variables (use IHS to deal with lots of zeros that we don't want to throw away http://marcfbellemare.com/wordpress/12856#more-12856
ihs <- function(x) { # Define inverse hyperbolic sine function
  y <- log(x + sqrt(x^2 + 1))
  return(y)
} 
# Transformations
parks$ihsx1213<-ihs(parks$X2012)
parks$ihsx1314<-ihs(parks$X2013)
parks$ihsx1415<-ihs(parks$X2014)
parks$ihsx1516<-ihs(parks$X2015)
parks$ihsPUD1213<-ihs(parks$PUD1213)
parks$ihsPUD1314<-ihs(parks$PUD1314)
parks$ihsPUD1415<-ihs(parks$PUD1415)
parks$ihsPUD1516<-ihs(parks$PUD1516)
parks$ihsxall<-ihs(parks$Xall)
parks$ihsPUDall<-ihs(parks$PUDall)
# 1km coast adjacent indicator as factor
parks$coast<-as.factor(parks$coast)

## Plot
# Basic plot of all data 
ggplot(parks, aes(x=ihsxall, y=ihsPUDall, color=coast)) + geom_point() + 
  labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
  theme_classic()
# By year
ggplot(parks, aes(x=ihsx1213, y=ihsPUD1213), color = "#f0f9e8") + geom_point() + 
  labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
  theme_classic()+
  geom_point(data=parks, aes(x=ihsx1314, y=ihsPUD1314), color = "#bae4bc") +
  geom_point(data=parks, aes(x=ihsx1415, y=ihsPUD1415), color = "#7bccc4") +
  geom_point(data=parks, aes(x=ihsx1516, y=ihsPUD1516), color = "#2b8cbe")
# Only coast adjacent
ggplot(parks %>% filter(coast == 1), aes(x=ihsxall, y=ihsPUDall)) + geom_point() + 
  labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
  theme_classic()
# Only comparisons where CDPR estimates are greater than zero
ggplot(parks %>% filter(ihsxall > 0), aes(x=ihsxall, y=ihsPUDall, color=coast)) + geom_point() + 
  labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
  theme_classic()
# Only comparisons where CDPR estimates are greater than zero for coastal parks
ggplot(parks %>% filter(ihsxall > 0 & coast ==1), aes(x=ihsxall, y=ihsPUDall, color=coast)) + geom_point() + 
  labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
  theme_classic()

## R2
# OLS of all observations
summary(lm(ihsPUDall ~ ihsxall, data = parks))
nobs(lm(ihsPUDall ~ ihsxall, data = parks))
# OLS of observations where CDPR estimates are greater than zero
summary(lm(ihsPUDall ~ ihsxall, data = parks %>% filter(ihsxall > 0)))
nobs(lm(ihsPUDall ~ ihsxall, data = parks %>% filter(ihsxall > 0)))
# OLS of observations where CDPR estimates are greater than zero and only coastal parks
summary(lm(ihsPUDall ~ ihsxall, data = parks %>% filter(ihsxall > 0 & coast == 1)))
nobs(lm(ihsPUDall ~ ihsxall, data = parks %>% filter(ihsxall > 0 & coast == 1)))

### LA Beaches (annual visitation, 2008 - 2010)
lab<-read.csv("./Data/LABeaches2010_2012.csv", header = TRUE)
# lab$ihsPUD<-ihs(lab$PUD)
# lab$ihsX<-ihs(lab$X)

# ggplot(parks %>% filter(ihsxall > 0 & coast ==1), aes(x=ihsxall, y=ihsPUDall)) + geom_point() + 
#   labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
#   theme_classic() +
#   geom_point(data=lab, aes(x=ihsX, y=ihsPUD), color = "#bae4bc")

### San Mateo County parks
smcp<-read.csv("./Data/SanMateoParks.csv", header = TRUE)
# smcp$ihsPUD<-ihs(smcp$PUD)
# smcp$ihsX<-ihs(smcp$visitor.counts)

# ggplot(parks %>% filter(ihsxall > 0 & coast ==1), aes(x=ihsxall, y=ihsPUDall)) + geom_point() + 
#   labs(x="CDPR visitation (IHS transformed)",y="PUD Flickr (IHS transformed)") +
#   theme_classic() +
#   geom_point(data=lab, aes(x=ihsX, y=ihsPUD), color = "#bae4bc") +
#   geom_point(data=smcp, aes(x=ihsX, y=ihsPUD), color = "blue") +
#   geom_smooth(method = lm, color = "black") +
#   geom_smooth(data = lab, formula = ihsX ~ ihsPUD, color = "#bae4bc")

### Combine data and plot
# The below also standardizes reporting, to annual visits/PUD
parkscoast0<-parks %>% filter(ihsxall > 0 & coast ==1) # Only using obs on the coast and >0 
d1<-select(parkscoast0, c(PUDall,Xall)) %>% st_set_geometry(NULL)
colnames(d1)<-c("PUD","X")
d1$group<-"CA Parks"
d1$PUD<-d1$PUD/4 # 4 years of data
d1$X<-d1$X/4
d2<-select(lab,c(PUD,X))
d2$group<-"LA Beaches"
d3<-select(smcp, c(PUD,visitor.counts))
colnames(d3)<-c("PUD","X")
d3$group<-"SMC Parks"
d3$PUD<-d3$PUD*4 # 3 months of data
d3$X<-d3$X*4
ds<-rbind(d1,d2,d3)

ds$ihsPUD<-ihs(ds$PUD)
ds$ihsX<-ihs(ds$X)

ds$group<-as.factor(ds$group)

ggplot(ds, aes(x=X, y=PUD, color = group)) + geom_point() + 
  labs(x="Surveyed total visitation",y="PUD Flickr") +
  theme_classic()

ggplot(ds, aes(x=ihsX, y=ihsPUD, color = group)) + geom_point() + 
  labs(x="Surveyed total visitation",y="PUD Flickr") +
  theme_classic() +
  geom_smooth(method = lm)

# CA Parks = summed total visitation, 2012 - 2016
# SMC Parks = summed visitation, April through June 2016
# LA Beaches = annual average visitation, 2008 - 2010

surveycomp<-ggplot(ds, aes(x=ihsX, y=ihsPUD)) + geom_point() + 
  labs(x="Surveyed total visitation",y="PUD Flickr") +
  theme_classic() +
  geom_smooth(method = lm)

# Ratio of X to PUD, all grouped
summary(mod<-lm(X ~ PUD, data = ds)) # Untransformed
# https://rpubs.com/iabrady/residual-analysis
plot(mod, which=1, col=c("blue")) # Residual plot
plot(mod, which=2, col=c("red")) # Q-Q plot
shapiro.test(mod[['residuals']])
plot(mod, which=3, col=c("blue"))  # Scale-Location Plot
plot(mod, which=5, col=c("blue"))  # Residuals vs Leverage
nobs(lm(X ~ PUD, data = ds))

summary(ihsmod<-lm(ihsX ~ ihsPUD, data = ds)) # Inverse hyperbolic sine transformation
plot(ihsmod, which=1, col=c("blue")) # Residual plot
plot(ihsmod, which=2, col=c("red")) # Q-Q plot
shapiro.test(ihsmod[['residuals']])
plot(ihsmod, which=3, col=c("blue"))  # Scale-Location Plot
plot(ihsmod, which=5, col=c("blue"))  # Residuals vs Leverage
nobs(lm(X ~ PUD, data = ds))

# QQ plot suggests that the residuals are not normally distributed using raw values, and that in particular there are issues with large values. IHS transformation returns normally distribute residuals
# Shapiro Wilk test for normality also supports this interpretation

rm(parks,PUD,lab,smcp,d1,d2,d3,mod,ihsmod)