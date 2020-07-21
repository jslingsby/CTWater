##################################################
### CoCT catchment infrastructure
##################################################
### Compiled by Jasper Slingsby
### 20 July 2020
##################################################

library(tidyverse)
library(readxl)
library(reshape2)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(cowplot)
library(wesanderson)
library(leaflet)
library(htmltools)
library(rosm)
library(ggspatial)
library(readxl)
library(leaflet)
library(htmltools)


###CoCT Tile servers
#https://citymaps.capetown.gov.za/agsext1/rest/services/Aerial_Photography_Cached/AP_2018_Feb/MapServer?f=jsapi
#http://emap.capetown.gov.za/agsext/rest/services/Aerial_Photography_Cached_Ext/AP_2015_Feb/ImageServer?f=jsapi

###Get DWS data

#NVD plots
NVD <- read.csv("/home/jasper/Documents/Datasets/NVD/NVD_16_09_2016/HeaderData_NVD2014.csv")
NVD <- NVD %>% filter(BiomeGIS == "F")
# NVD <- NVD %>% filter(Long > 18.773 & 
#                          Long < 19.313 &
#                          Lat < -33.883 &
#                          Lat > -34.229) # &
                        # `Open / Closed` == "Open")

pname <- paste0(NVD$ORIG_DB, "_", NVD$PlotYear)
pdat <- data.frame(lat = NVD$Lat, lon = NVD$Long)

#Water quantity
wqn <- read_xlsx("/home/jasper/Documents/Datasets/DWS Network/Copy of NWRM_Data Catalogue_20150430.xlsx", sheet = 1, skip = 7)

# wqns <- wqn %>% filter(Longitude > 18.773 & 
#                          Longitude < 19.313 & 
#                          Latitude < -33.883 & 
#                          Latitude > -34.229 &
#                          `Open / Closed` == "Open")

#c(18.773, 19.313, -34.229, -33.883)

wqb <- wqn %>% filter(WMA %in% c("Berg/Olifants", "Breede/Gouritz") &
                         `Open / Closed` == "Open") 

#Boreholes
bname <- paste0(wqb$`Station Type`, " <br/> ", "(DWS ", wqb$`Station Number`, ")")
bdat <- data.frame(lat = wqb$Latitude, lon = wqb$Longitude)


wqns <- wqn %>% filter(WMA %in% c("Berg/Olifants", "Breede/Gouritz") &
                      `Open / Closed` == "Open" &
                        `Station Type` != "Borehole") 

wqname <- paste0(wqns$`Station Type`, " <br/> ", "(DWS ", wqns$`Station Number`, ")")
wqdat <- data.frame(lat = wqns$Latitude, lon = wqns$Longitude)

#Hydromet
met <- read_xlsx("/home/jasper/Documents/Datasets/DWS Network/Copy of NWRM_Data Catalogue_20150430.xlsx", sheet = 4, skip = 7)
mets <- met %>% filter(WMA %in% c("Berg/Olifants", "Breede/Gouritz") &
                         `Open / Closed` == "Open") 
mtname <- paste0(mets$`Station Type`, " <br/> ", "(", mets$Source, " ", mets$`Station Number`, ")")
mtdat <- data.frame(lat = mets$Latitude, lon = mets$Longitude)

###Get Jonkershoek data
dat <- read_xlsx("/home/jasper/Dropbox/SAEON/Management/Website/SAEON_field instrument list.xlsx", sheet = 1)

mdat <- dat[which(dat$Site == "Jonkershoek" & dat$Owner == "SAEON"  & dat$Class %in% c("Meteorological", "Micro meteorological")),]
mname = paste0(mdat$`Station name`, " <br/> ", "(", mdat$Variables, ")")
mdat <- data.frame(lat = mdat$Latitude, lon = mdat$Longitude)

hdat <- dat[which(dat$Site == "Jonkershoek" & dat$Owner == "SAEON"  & dat$Class == "Hydrological"),]
hname = paste0(hdat$`Station name`, " <br/> ", "(", hdat$Variables, ")")
hdat <- data.frame(lat = hdat$Latitude, lon = hdat$Longitude)

###Group sets
#Surface Water
hdat <- rbind(hdat, wqdat)
hname <- c(hname, wqname)

#Meteorology
mdat <- rbind(mdat, mtdat)
mname <- c(mname, mtname)

#offering <- c("Hourly Rainfall and Temperature", "Hourly Rainfall, Temperature, RH, Wind, Solar Rad", 	"Hourly Rainfall and Temperature")

###Plot the map
leaflet() %>%
  addTiles(group = "Default") %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(provider = "Esri.WorldImagery", group = "Satellite") %>%
  # addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topo") %>%

  addCircleMarkers(data = pdat,
                   label = 
                     lapply(pname, HTML), 
                   group = "Vegetation",
                   radius = 3, 
                   color = "green") %>%
  
  addCircleMarkers(data = bdat,
                   label = 
                     lapply(bname, HTML), 
                   group = "Groundwater",
                   radius = 3, 
                   color = "purple") %>%
  
  addCircleMarkers(data = mdat,
                   label = 
                     lapply(mname, HTML), 
                   group = "Meteorological",
                   radius = 3) %>%
  
  addCircleMarkers(data = hdat,
                   label = 
                     lapply(hname, HTML), 
                   group = "Surface water",
                   radius = 3, 
                   color = "red") %>%
  
  addLayersControl(overlayGroups =
                     c("Vegetation", "Meteorological", "Surface water", "Groundwater"),
                     baseGroups =
                       c("Default", "Satellite") ,
                   options = layersControlOptions(collapsed = FALSE)) #%>%
#  hideGroup(c("Default", "Satellite"))
