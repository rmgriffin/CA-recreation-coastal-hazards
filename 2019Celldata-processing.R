# Setup -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

#system("sudo apt install libgeos-dev libproj-dev libgdal-dev libudunits2-dev -y") # Install linux geospatial dependencies 

# Install/call libraries
#install.packages("renv")
#renv::init()

PKG <- c("googledrive","tidyverse", "rgdal","raster","sf","furrr","data.table","filesstrings","rnaturalearth")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

renv::snapshot()
rm(p,PKG)

## Cell data
# Mass shapefiles https://docs.digital.mass.gov/dataset/massgis-data-state-outlines
# Cell data is from AirSage
dir.create(file.path('Data'), recursive = TRUE)
folder_url<-"https://drive.google.com/open?id=1egzpicB1TTFB7ZNLd22GZHgratbZPFcm"
folder<-drive_get(as_id(folder_url))
files<-drive_ls(folder)
dl<-function(files){
  walk(files, ~ drive_download(as_id(.x), overwrite = TRUE))
}
setwd("./Data")
system.time(map(files$id,dl))
system.time(unzip("2019Celldata.zip", exdir = "."))
file.remove("2019Celldata.zip")
file.remove("MA_line.gpkg")
file.remove("MA_poly.gpkg")
setwd("..")
rm(files, folder, folder_url, dl)

## Processing
Cvg<-st_read("./Data/Coverage.gpkg")
Cvg<-st_transform(Cvg,crs = 4326)
C01<-read_csv("./Data/nation-wide-shorelines-201901.csv",col_names = TRUE)
C02<-read_csv("./Data/nation-wide-shorelines-201902.csv",col_names = TRUE)
C03<-read_csv("./Data/nation-wide-shorelines-201903.csv",col_names = TRUE)
C04<-read_csv("./Data/nation-wide-shorelines-201904.csv",col_names = TRUE)
C05<-read_csv("./Data/nation-wide-shorelines-201905.csv",col_names = TRUE)
C06<-read_csv("./Data/nation-wide-shorelines-201906.csv",col_names = TRUE)
C07<-read_csv("./Data/nation-wide-shorelines-201907.csv",col_names = TRUE)
C08<-read_csv("./Data/nation-wide-shorelines-201908.csv",col_names = TRUE)
C09<-read_csv("./Data/nation-wide-shorelines-201909.csv",col_names = TRUE)
C10<-read_csv("./Data/nation-wide-shoreline-201910.csv",col_names = TRUE)
C11<-read_csv("./Data/nation-wide-shorelines-201911.csv",col_names = TRUE)
C12<-read_csv("./Data/nation-wide-shorelines-201912.csv",col_names = TRUE)

C<- C01 %>% 
  full_join(C02, by = c("lat_bin","lon_bin")) %>% 
  full_join(C03, by = c("lat_bin","lon_bin")) %>% 
  full_join(C04, by = c("lat_bin","lon_bin")) %>% 
  full_join(C05, by = c("lat_bin","lon_bin")) %>% 
  full_join(C06, by = c("lat_bin","lon_bin")) %>% 
  full_join(C07, by = c("lat_bin","lon_bin")) %>% 
  full_join(C08, by = c("lat_bin","lon_bin")) %>% 
  full_join(C09, by = c("lat_bin","lon_bin")) %>% 
  full_join(C10, by = c("lat_bin","lon_bin")) %>% 
  full_join(C11, by = c("lat_bin","lon_bin")) %>% 
  full_join(C12, by = c("lat_bin","lon_bin"))

rm(C01,C02,C03,C04,C05,C06,C07,C08,C09,C10,C11,C12)
C<-C %>% dplyr::select(2,3,4,6,8,10,12,14,16,18,20,22,24,26)
colnames(C)<-c("lat_bin","lon_bin","DC1","DC2","DC3","DC4","DC5","DC6","DC7","DC8","DC9","DC10","DC11","DC12")
C[is.na(C)]<-0
C$DC_YR<-rowSums(C[3:14])

D<-st_as_sf(C, coords = c("lon_bin", "lat_bin"),crs = 4326, agr = "constant")

st_write(D,"./Data/DC2019.gpkg")

# # Plotting Template
# a<-ggplot() +
#   geom_sf(data=sm, color = "tan") +
#   geom_sf(data=rd.sc, color = "grey") +
#   geom_sf(data = PUD500m, aes(fill=PUD_b)) +
#   scale_fill_brewer(palette = "Oranges", na.translate = FALSE, guide = guide_legend(override.aes = list(linetype = "blank"), title = "Flickr User Days")) + # https://github.com/tidyverse/ggplot2/issues/2763
#   geom_sf(data=ESI, aes(color="ESI Line"), show.legend = TRUE) +
#   scale_color_manual(values = c("ESI Line" = "black"), name = "") +
#   geom_label_repel(data = cdp, aes(label=NAME, geometry = geometry), stat = "sf_coordinates", min.segment.length = 0, size=2.5, point.padding = NA) +
#   coord_sf(xlim=c(-122.5402,-122.3882),ylim=c(37.38338,37.5586), datum = NA) +
#   theme_void() +
#   theme(legend.position = c(0.25, 0.32), legend.margin = margin(-0.25,0,0,0, unit="cm"), panel.background = element_rect(fill = "aliceblue", color = NA), axis.title.x=element_blank(), axis.title.y=element_blank(), legend.text=element_text(size=7), legend.title=element_text(size=8), plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")) +
#   xlab(NULL) + 
#   ylab(NULL)