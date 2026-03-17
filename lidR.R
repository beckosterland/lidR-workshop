########################################################################

#   LiDAR Tutorial using lidR package for urban forestry applications

########################################################################

# Beck Osterland
# April 7, 2022

# Jean-Romain Roussel and David Auty (2022). Airborne LiDAR Data Manipulation and Visualization
# for Forestry Applications. R package version 4.0.0. https://cran.r-project.org/package=lidR

# Joe Cheng, Bhaskar Karambelkar and Yihui Xie (2022). leaflet: Create Interactive Web Maps with
# the JavaScript 'Leaflet' Library. R package version 2.1.1.
# https://CRAN.R-project.org/package=leaflet

# Mark Padgham, Bob Rudis, Robin Lovelace, Ma?lle Salmon (2017).
# osmdata Journal of Open Source Software, 2(14). URL
# https://doi.org/10.21105/joss.00305

#Citation function
citation("lidR")
citation("leaflet")
citation("osmdata")

# INSTALLS
install.packages("lidR")
#install.packages("leaflet")
#install.packages("sf")
#install.packages("htmlwidgets")
#install.packages("osmdata")

# Load libraries
library(lidR)
library(leaflet)
library(sf)
library(htmlwidgets)
library(osmdata)
library(tidyverse)

#Set working directory
setwd("E:/COGS/city_DEMs/MontrealLIDAR")

#Load one LAZ file
# La Fontaine Park in Montreal
las <- readLAS("297-5038_2015.laz")

#Visualize point cloud
plot(las)

# Vancouver
sp <- readLAS("E:/COGS/city_DEMs/Vancouver_StanleyPark/4900E_54600N.las")
plot(sp, color = "Intensity")

# Read multiple LAS/LAZ files into a Catalogue
laf <- readLAScatalog("E:/COGS/city_DEMs/MontrealLIDAR/ParcLafontaine_LiDAR")

#View specs
laf

#plot extent
plot(laf)

#Check quality
las_check(laf)

# Create a Circular Region of Interest, overlapping two LAZ files
# 400 radius
roi <- clip_circle(laf, x = 299392.55, y = 5042872.91, radius = 400)
plot(roi, color = "Classification")

###########################################################
###     Retrieve OpenStreetMap Data directly from R    ###
#        osmdata package

#Build Query to find parks in Montr?al
# https://wiki.openstreetmap.org/wiki/Map_features
q <- getbb("?le de Montr?al") %>% #get bounding box for 'name'
  opq() %>% #build query
  add_osm_feature("leisure", "park") #specify attributes (tags) you wish to query

#Run query, accesses OSM server
#Outputs a sf object
parks <- osmdata_sf(q)

#Get only polygon features
parcs_mtl <- parks$osm_polygons

#Make Leaflet Map
m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(minZoom = 1)) %>%
  addPolygons(data = parcs_mtl, popup = paste0("Parc: ", parcs_mtl$name),
              fillColor = "green", color = "#9ce340")

m

#Filter to Parc La Fontaine
lafontaine <- parcs_mtl %>%
  filter(name == "Parc La Fontaine")

#Reproject Parc La Fontaine sf layer to NAD83 MTM Zone 8
#Use epsg.io to find projection definition information
lafontaine <- st_transform(lafontaine, "+proj=tmerc +lat_0=0 +lon_0=-73.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

##  Back To LIDAR
#Clip LiDAR points to polygon of La Fontaine Park
roi <- clip_roi(laf, lafontaine)
plot(roi)

# New variable that filters Points by their Classification
# 5L is the Classification number for the High Vegetation class
roi_highveg <- filter_poi(roi, Classification == 5L)
plot(roi_highveg, color = "Classification")

library(terra)
# Create DSM
dsm <- rasterize_canopy(roi, res = 0.5, algorithm = p2r())
plot(dsm)

#Create CHM
chm <- rasterize_canopy(roi_highveg, res = 0.5, pitfree(subcircle = 0.2))
plot(chm)

# Locate tree tops
ttops <- locate_trees(roi_highveg, lmf(ws=11))



# Plot in 3D
x <- plot(roi)
add_treetops3d(x, ttops)


##     Leaflet Mapping   ##

#Reproject ttops sf from MTM zone 8 to WGS84
# Use st_transform
treetops <- st_transform(ttops, "+proj=longlat +datum=WGS84 +no_defs")

# Define color palette based on data (tree height)
pal2 <- colorNumeric(palette = "YlGn", domain=treetops$Z, na.color="white")

# Import PNG icon of tree
tree_icon <- makeIcon(iconUrl = "E:/COGS/city_DEMs/img/tree.png",
                      iconWidth = 20)

# Create leaflet Map
m <- leaflet(treetops) %>%
  # https://leaflet-extras.github.io/leaflet-providers/preview/
  addProviderTiles(providers$OpenStreetMap.HOT, # Add basemap
                   options = providerTileOptions(minZoom = 1)) %>%
  
  addCircleMarkers(data = treetops, fillColor = ~pal2(Z), fillOpacity = 1,
                   opacity = 0.2, radius = ~sqrt(Z), 
                   popup = paste("Tree Height: ", '<b>', (treetops$Z - 42), '</b>'))

m

htmlwidgets::saveWidget(m, "leafletRmap.html")


#Leaflet map with "TREE.png" as a marker
##### KINDA BUGGY  #######
e <- leaflet(treetops) %>%
  addProviderTiles(providers$OpenStreetMap.HOT,
                   options = providerTileOptions(minZoom = 1)) %>%
  
  addMarkers(data = treetops, icon = tree_icon, 
                   popup = paste("Tree Height: ", '<b>', (treetops$Z - 42), '</b>'))

e


## Shiny App
library(shiny)
library(sf)
library(leaflet)

shp <- read_sf('contours.shp')
library(raster)
library(tidyverse)

#Sets the layout for you shiny page
ui <- fluidPage(leafletOutput("LeafletMap"))

server <- function(input, output){
  
  #function which defines the leaflet map
  output$LeafletMap<-
    renderLeaflet(leaflet() %>% 
                    addTiles() %>% 
                    addPolylines(data=shp, color = shp$ELEV
                    ))
}

shinyApp(ui = ui, server = server)
