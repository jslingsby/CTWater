##################################################
### Species threatened by Aquifer abstraction
##################################################
### Compiled by Jasper Slingsby
### 18 September 2019
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

# #Get vegmap - replace with latest?
# vegmap <- readOGR(dsn = "/Users/jasper/Documents/GIS/South Africa/NVM2012_Wgs84_Geo_06072017/NVM2012_Wgs84_Geo_06072017.shp", layer = "NVM2012_Wgs84_Geo_06072017")
# 
# #Get protected areas
# pa <- readOGR(dsn = "/Users/jasper/Documents/GIS/VegToolsRaw/PA/PA14_terUTM34s.shp", layer = "PA14_terUTM34s")
# 
# pa <- spTransform(pa, "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# 
# # #Get land cover
# # lc <- raster("/Users/jasper/Documents/GIS/Landcover/NLC  Degradation/Fynbs_NLC1.tif")
# # lc <- lc<2
# 
# #Get geology
# geo <- readOGR(dsn = "/Users/jasper/Documents/GIS/Geology/CFB_Lithology Maps (665Mb)/1_250000/New Folder/GeologyWGS1984.shp", layer = "GeologyWGS1984")

####Get borehole site data and project to UTM34S
#bh <- readOGR(dsn = "/home/jasper/Dropbox/SAEON/Projects/TMGA/boreholesites/TMGA.kml", layer = "TMGA") #Originals
#bhb <- spTransform(bh, "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#bh35 <- spTransform(bh, "+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#Switch names for plots or group for impact figure
sbh <- readOGR(dsn = "/home/jasper/Dropbox/SAEON/Projects/TMGA/boreholesites/Steenbras.kml", layer = "Steenbras")
gbh <- readOGR(dsn = "/home/jasper/Dropbox/SAEON/Projects/TMGA/boreholesites/Groenlandberg core Sept19.kml", layer = "Groenlandberg core Sept19")
nbh <- readOGR(dsn = "/home/jasper/Dropbox/SAEON/Projects/TMGA/boreholesites/Nuweberg.kml", layer = "Nuweberg")

bh <- rbind(sbh, gbh, nbh)
bhb <- spTransform(bh, "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

                                  
####Get threatened species data and project to UTM34S
#tspp <- readOGR(dsn = "/home/jasper/Dropbox/SAEON/Projects/TMGA/SANBI_Scoping/CREW Survey Priorities/AllTOCCs_2017.shp", layer = "AllTOCCs_2017")
tspp <- read_delim("/home/jasper/Dropbox/SAEON/Projects/SANBI/ThreatenedSpecies/WC_TOCC.txt", delim = "\t")
coordinates(tspp) <- ~ Long + Lat
proj4string(tspp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#writeOGR(tspp["Taxon"], dsn = "/home/jasper/Dropbox/SAEON/Projects/SANBI/ThreatenedSpecies/tspp.kml", layer="tspp", driver="KML")

tsp_rare <- tspp[which(tspp$'NATIONAL STATUS'%in% c("Critically Rare", "Rare")),]
#writeOGR(tsp_rare["Taxon"], dsn = "/home/jasper/Dropbox/SAEON/Projects/SANBI/ThreatenedSpecies/tsp_rare.kml", layer="tsp_rare", driver="KML")

tsppp <- spTransform(tspp, "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#tsp35 <- spTransform(tspp, "+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tsppp$UID <- 1:nrow(tsppp@data)

tsp_rare <- tsppp[which(tsppp$'NATIONAL STATUS'%in% c("Critically Rare", "Rare")),]


####Make density layer
#ext <- extent(18.75, 19, -34.22, -34.14) #Steenbras
ext <- extent(bh) #Steenbras, Nuweberg, Groenlandberg

alt <- raster("/home/jasper/Dropbox/SAEON/Projects/EMSAfrica/GJAM_data/altitude")
alt <- crop(alt, ext)
alt <- disaggregate(alt, 4)
tdens <- rasterize(tspp, alt, field = "Taxon", fun = "count")
#tdens <- crop(tdens, ext)
tdensp <- projectRaster(tdens, crs = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))
tdensd <- as.data.frame(rasterToPoints(tdensp, spatial = F))
colnames(tdensd)[3] <- "Number of species"

talt <- crop(alt, ext) #extent(18.25, 19.25, -34.5, -33.8))
talt <- as.data.frame(rasterToPoints(talt, spatial = F))
colnames(talt)[3] <- "elevation"

bhc <- as.data.frame(coordinates(spTransform(bh, "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")))

register_tile_source(light = "http://a.basemaps.cartocdn.com/light_nolabels/${z}/${x}/${y}.png")

tdensr <- ggplot() + 
  annotation_map_tile(type = "light") + #"light" +
  geom_raster(data=tdensd, aes(x, y, fill=`Number of species`)) +
  #scale_fill_gradient_tableau("Red", name = "Probability") +
  scale_fill_gradient(low = alpha("white",0.5) , high = alpha("red4",0.5), name = "Number of species") +
 # geom_contour(data=talt, aes(x, y, z=elevation), colour = "black", breaks = seq(100,1500,100), linemitre = 1, alpha = 0.5) +
#  coord_fixed(1.3) + 
  geom_point(data = bhc, aes(x = coords.x1, y = coords.x2)) +
  theme_bw() +
  theme(axis.line=element_blank(),
      # axis.text.x=element_blank(),
      # axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) #+
#  annotation_north_arrow(mapping = aes(location = "bl"), height = unit(.75, "cm"), width = unit(.75, "cm")) +
#  annotate("text", x = 264000, y = 6231000, label = "2 km") +
#  annotate("segment", x = 263000, xend = 265000, y = 6231500, yend = 6231500)

tdensr

####Make map

#leaflet(tspp) %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
#  addMarkers(lng = tspp$Long, lat = tspp$Lat) #, popup = ~htmlEscape(Taxon))


####Get wetland spp (from Erwin)
wspp <- read_xlsx("/home/jasper/Dropbox/SAEON/Projects/TMGA/Paper/Threat of Groundwater/Obligate_wetland_species.xlsx", sheet = 1)
owspp <- wspp[grep("Obligate wetland plant", wspp$WetlandDependence...15),]
ofwspp <- wspp[grep("wetland", wspp$WetlandDependence...15),]

length(which(wspp$`Species name`%in%tspp$Taxon))
length(which(owspp$`Species name`%in%tspp$Taxon))
length(which(ofwspp$`Species name`%in%tspp$Taxon))



# #Boreholes by PA
# pad <- over(bhb, pa)
# sum(!is.na(pad[,1])) # in protected areas
# pad$Name <- as.character(bhb$Name)
# 
# #Boreholes by veg type
# veg <- over(bh, vegmap)
# veg$Name <- as.character(bh$Name)
# 
# #Boreholes by land cover
# #lcd <- over(bhb, lc)
# lcd <- raster::extract(lc, coordinates(bh35)[,1:2])
# 
# #lcs <- crop(lc, bh35, snap = "out")
# #lcd <- extract(lcs, bh35) #coordinates(bh35)[1:2])
# #SpatialPoints(coordinates(bhb)[1:2], proj4string=CRS(proj4string(lc))))
# 
# #Merge PA and veg type results
# out <- merge(pad, veg)
# out$Natural <- lcd
# pen <- out[substr(out$Name,1,2) %in% c("CB", "MB", "RD", "SM", "SW"),]
# penout <- pen[,c(1,2,5,10)]
# #write.csv(penout, "/Users/jasper/Dropbox/SAEON/Projects/TMGA/Meetings/29_March_EWG_Screening_tool/screeningtool_v0.1/SPD.csv")



#Buffer boreholes with set distance (in m)
#bhb1000 <- gBuffer(bhb, byid = TRUE, width = 1000)



buffs <- seq(10, 500, 10)

out <- list() #as.data.frame(matrix(NA, length(buffs))

for(i in 1:length(buffs)) {
bhb100 <- gBuffer(bhb, byid = TRUE, width = buffs[i])

#Extract all "Rare" and "Critically Rare" species occurring within the borehole buffer zones and simplify data
int100 <- over(bhb100, tsppp, returnList = T)
names(int100) <- bhb100@data$Name
intu100 <- do.call(rbind, int100)
intu100$drill <- sapply(rownames(intu100), function(x){strsplit(x, split = "\\.")[[1]][1]})

out[[i]] <- intu100
}

names(out) <- buffs
out <- do.call(rbind, out)
out$buffer <- sapply(rownames(out), function(x){strsplit(x, split = "\\.")[[1]][1]})

dat <- out %>% group_by(buffer, `NATIONAL STATUS`) %>% summarise(nSpp = n_distinct(Taxon), nPopln = n())
dat$buffer <- as.numeric(dat$buffer)
dat <- melt(dat, id = c("buffer", "NATIONAL STATUS")) 
dat <- dat[-which(dat$`NATIONAL STATUS`=="CR PE"),]

#pal <- wes_palette(7, name = "Zissou1", type = "continuous")
pal <- wes_palette(5, name = "Zissou1")
#image(volcano, col = pal)
#pal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colourblind palette
pal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7") #colourblind palette


g <- ggplot(dat) +
  geom_area(aes(y = value, x = buffer, fill = `NATIONAL STATUS`)) +
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  scale_fill_manual(values=rev(pal)) +
  xlab("Buffer distance (m)") +
  ylab ("Number")

gl <- ggplot(dat) +
  geom_line(aes(y = value, x = buffer, colour = `NATIONAL STATUS`)) +
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  scale_colour_manual(values=rev(pal)) +
  xlab("Buffer distance (m)") +
  ylab ("Number")

g

gl

##########################
gspp <- ggplot(spp) +
  geom_line(aes(y = nSpp, x = buffer, colour = `NATIONAL STATUS`)) 

pop <- out %>% group_by(buffer, `NATIONAL STATUS`) %>% summarise(nPopln = n())
pop$buffer <- as.numeric(pop$buffer)

gpop <- ggplot(pop) +
  geom_line(aes(y = nPopln, x = buffer, colour = `NATIONAL STATUS`))


impact <- ggdraw() +
  draw_plot(gspp, x = 0, y = 0, width = .5, height = 1) + 
  draw_plot(gpop, x = 0.5, y = 0, width = .5, height = 1)

# length(intu100$Taxon) # populations
# length(unique(intu100$Taxon)) # species
# length(intu100$`NATIONAL STATUS` == "DDD")
# length(intu100$`NATIONAL STATUS` == "Rare")
# length(intu100$`NATIONAL STATUS` == "Critically Rare")
# length(intu100$`NATIONAL STATUS` == "NT")
# length(intu100$`NATIONAL STATUS` == "VU")
# length(intu100$`NATIONAL STATUS` == "EN")
# length(intu100$`NATIONAL STATUS` == "CR")
# length(intu100$`NATIONAL STATUS` == "CR PE")
# length(intu100$`NATIONAL STATUS` == "EW")


#Extract all threatened species occurring within the borehole buffer zones and simplify data
int1000 <- over(bhb1000, tsppp, returnList = T)
names(int1000) <- bhb1000@data$Name
intu1000 <- do.call(rbind, int1000)
intu1000$drill <- sapply(rownames(intu1000), function(x){strsplit(x, split = "\\.")[[1]][1]})

#calculate spp eliminated
x <- tsppp@data
x <- as.data.frame(sapply(x, as.character), stringsAsFactors = F)

length(unique(x$Taxon)) # # of spp in database
length(unique(x$Taxon[!x$UID %in% unique(intu1000$UID)])) # # of spp in database not entirely eliminated by drill points

unique(x$Taxon)[!unique(x$Taxon) %in% unique(x$Taxon[!x$UID %in% unique(intu1000$UID)])]


tsppp$Taxon %in% unique(intu1000$Taxon)

dim(unique(intu1000[,-which(colnames(intu1000) == "drill")])) # of populations threatened
dim(unique(intu1000)) # of drill by population intersections
length(unique(intu1000$Taxon)) # of spp threatened
sum(sapply(int1000, nrow)==0)




#Peninsula only
Pintu1000 <- intu1000[c(grep("CB", intu1000$drill), grep("MB", intu1000$drill), grep("RD", intu1000$drill), grep("SM", intu1000$drill), grep("SW", intu1000$drill)),]

dim(unique(Pintu1000[,-which(colnames(Pintu1000) == "drill")])) # of populations threatened
dim(unique(Pintu1000)) # of drill by population intersections
length(unique(Pintu1000$Taxon)) # of spp threatened

#Buffer boreholes with set distance (in m)
bhb500 <- gBuffer(bhb, byid = TRUE, width = 500)

#Extract all threatened species occurring within the borehole buffer zones and simplify data
int500 <- over(bhb500, tsppp, returnList = T)
names(int500) <- bhb500@data$Name
intu500 <- do.call(rbind, int500)
intu500$drill <- sapply(rownames(intu500), function(x){strsplit(x, split = "\\.")[[1]][1]})

dim(unique(intu500[,-which(colnames(intu500) == "drill")])) # of populations threatened
dim(unique(intu500)) # of drill by population intersections
length(unique(intu500$Taxon)) # of spp threatened
sum(sapply(int500, nrow)==0)

#Buffer boreholes with set distance (in m)
bhb200 <- gBuffer(bhb, byid = TRUE, width = 200)

#Extract all threatened species occurring within the borehole buffer zones and simplify data
int200 <- over(bhb200, tsppp, returnList = T)
names(int200) <- bhb200@data$Name
intu200 <- do.call(rbind, int200)
intu200$drill <- sapply(rownames(intu200), function(x){strsplit(x, split = "\\.")[[1]][1]})

dim(unique(intu200[,-which(colnames(intu200) == "drill")])) # of populations threatened
dim(unique(intu200)) # of drill by population intersections
length(unique(intu200$Taxon)) # of spp threatened
sum(sapply(int200, nrow)==0)

###

spp <- read.csv("/Users/jasper/Dropbox/SAEON/Projects/TMGA/SANBI_Scoping/Tocc_1kmborehole_habitats.csv") # Give the file name

n <- spp %>% group_by(Taxon) %>% summarise(n = n())
