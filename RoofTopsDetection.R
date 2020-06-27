library(lidR)
library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(viridisLite)
library(rasterVis)
library(grid)
library(scales)
library(RStoolbox)


############################### I. study area Indiana USA : Indiana 1
lidar1<- readLAS("data1/Indiana1.laz")
lidar1
plot(lidar1)

extent(lidar1)
#
# Lets explore the data 
############################### 1. Digital Terrain Model  ##############################################
?grid_terrain()
col <- height.colors(50)
dtm1 = grid_terrain(lidar1, algorithm = knnidw(k = 6L, p=2))
dtm1
dtm2 = grid_terrain(lidar1, algorithm = tin())
dtm2
dtm3 = grid_terrain(lidar1, algorithm = kriging(k = 10L))
dtm3

## Not run:

plot(dtm1,main="DTM1")
plot(dtm2, main="DTM2")
plot(dtm3, main="DTM3")

#3D ViZ
plot_dtm3d(dtm1)
plot_dtm3d(dtm2)
plot_dtm3d(dtm3)

################################# 2. Digital Surface Model ####################################
# the function to Create a canopy surface model (i.e. canopz height model, CHM) using a point cloud. For each pixel the function returns
#the highest point found (point-to-raster)
?grid_canopy
col <- height.colors(50)

## Points-to-raster algorithm with a resolution of 1 meter
DSM1 = grid_canopy(lidar1, res= 1, algorithm = p2r())
DSM1
plot(DSM1,col=col ,main= "DSM 1")

# Local maximum algorithm with a resolution of 1 meter replacing each
# point by a 20 cm radius circle of 8 points
DSM2 = grid_canopy(lidar1, res=1, algorithm = p2r(subcircle = 0.2))
DSM2
plot(DSM2,col=col, main="DSM 2")

# Points-to-raster algorithm with a resolution of 0.5 meters replacing each
# point by a 20-cm radius circle of 8 points
DSM3 <- grid_canopy(lidar1, res = 0.5, p2r(0.2))
DSM3
plot(DSM3, col = col, main="DSM 3")

#3D VIZ
plot_dtm3d(DSM1)
plot_dtm3d(DSM2)
plot_dtm3d(DSM3)


######## nDSM 
nDSM1<- (DSM1 - dtm1)
nDSM1

plot(nDSM1, main="nDSM1")
plot_dtm3d(nDSM1)

writeRaster(nDSM1, "data1/nDSM1.tiff")

######## Map the pulse or point density  
# function to spatially map the pulse or point density in a lidar point cloud
?grid_density
#Point density Map with Grid size of 1
d = grid_density(lidar1, res=1)
plot(d, main="Pulse at 1m grid size")

#Point density Map with Grid size of 5
d = grid_density(lidar1, res=5)
plot(d, main="Pulse at 5m grid size")


######### lasfilter:  Return points with matching conditions 
?lasfilter

# Select the first returns classified as ground
firstground1 = lasfilter(lidar1, Classification == 2L & ReturnNumber == 1L)
firstground1
plot(firstground1)


######## Classify points as 'ground' or 'not ground' 
# this function classifies the point cloud into ground and non-ground points
?lasground
rad1 = readLAS("data1/Indiana1.laz", select = "xyzrn")
rad1
plot(rad1)

# 1/Using the Progressive Morphological Filter
# --------------------------------------

ws  <- seq(3,12, 3)
th  <- seq(0.1, 1.5, length.out = length(ws))

rad1 <- lasground(rad1, pmf(ws, th)) #pmf(): a ground-segmentation function

rad1

plot(rad1, color = "Classification") # >> the flat rooftops are poorely classified!!!

#' 2/# Using the Cloth Simulation Filter
# --------------------------------------

# (Parameters chosen mainly for speed)
mycsf <- csf(TRUE, 1, 1, time_step = 1)
rad2 = readLAS("data1/Indiana1.laz", select = "xyzrn")
rad2
rad2 <- lasground(rad2, mycsf)
plot(rad2, color = "Classification") #>> the flat rooftops are poorely classified!!!


######## Estimation of the shape of the points neighborhood

?lasdetectshape()
lidar1 <- lasdetectshape(lidar1, shp_plane(k = 15), "Coplanar")
plot(lidar1, color = "Coplanar")

# Drop ground point at runtime
lidar1 <- lasdetectshape(lidar1, shp_plane(k = 15), "Coplanar", filter = ~Classification != 2L)
plot(lidar1, color = "Coplanar")

##### OPEN QUESTION!!!
# 1.How to classify the vegeation and the buildings properly? (the Heigth doesn't make sense here!)
# 2. We need to drop the vegetation so that we end up we the buildings points cloud only
# 3.IS lidR feasible to answer these Questions?

##### what kind of progress has been made so far 
# 1. compution of DTM, DSM, and nDSM 
# 2. We extrated the buildings footprint from OSM on study area Indiana1
# 3. computing the slope : we need only the slope with a range of (0-5??)

### >> 1. Open Issues: if we clip the nDSM based on the buildings OSM footprints : we neglect the fact that some point cloud 
# (close enougth to the edges of the buildings) generated from nDSM belongs normally to the vegeation class and not the buildings 
# 2. OSM Buildings footprint doesn't include some of the actual Buildings!! Not reliable?

############################### II. study area Indiana USA : Indiana 2
lidar2<- readLAS("data1/Indiana2.laz")
lidar2
plot(lidar2)

extent(lidar2)

######## DTM
col <- height.colors(50)
dtm = grid_terrain(lidar2, algorithm = knnidw(k = 6L, p=2))
dtm

plot(dtm,main="DTM")

# 3D viz
plot_dtm3d(dtm)

######## DSM
?grid_canopy
col <- height.colors(50)

DSM = grid_canopy(lidar2, res= 1, algorithm = p2r())
DSM
plot(DSM,col=col ,main= "DSM")

#3D VIZ
plot_dtm3d(DSM1)

######## nDSM 
nDSM2<- (DSM - dtm)
nDSM2

plot(nDSM2, main="nDSM2")
plot_dtm3d(nDSM2)

writeRaster(nDSM2, "data1/nDSM2.tiff")

######## Map the pulse or point density  
?grid_density
#Point density Map with Grid size of 1
d = grid_density(lidar2, res=1)
plot(d, main="Pulse at 1m grid size")

#Point density Map with Grid size of 5
d = grid_density(lidar2, res=5)
plot(d, main="Pulse at 5m grid size")


######### lasfilter:  Return points with matching conditions 
?lasfilter

# Select the first returns classified as ground
firstground2 = lasfilter(lidar2, Classification == 2L & ReturnNumber == 1L)
firstground2
plot(firstground2)

######## Classify points as 'ground' or 'not ground' 
# this function classifies the point cloud into ground and non-ground points
?lasground
rad101 = readLAS("data1/Indiana2.laz", select = "xyzrn")
rad101
plot(rad101)

# 1/Using the Progressive Morphological Filter
# --------------------------------------

ws  <- seq(3,12, 3)
th  <- seq(0.1, 1.5, length.out = length(ws))

rad101 <- lasground(rad101, pmf(ws, th)) #pmf(): a ground-segmentation function

rad101

plot(rad101, color = "Classification") # >> the flat rooftops are poorely classified!!!

#' 2/# Using the Cloth Simulation Filter
# --------------------------------------

# (Parameters chosen mainly for speed)
mycsf <- csf(TRUE, 1, 1, time_step = 1)
rad202 = readLAS("data1/Indiana2.laz", select = "xyzrn")
rad202
rad202 <- lasground(rad202, mycsf)
plot(rad202, color = "Classification") #>> the flat rooftops are poorely classified!!!


######## Estimation of the shape of the points neighborhood

?lasdetectshape()
lidar2 <- lasdetectshape(lidar2, shp_plane(k = 15), "Coplanar")
plot(lidar2, color = "Coplanar")

# Drop ground point at runtime
lidar1 <- lasdetectshape(lidar1, shp_plane(k = 15), "Coplanar", filter = ~Classification != 2L)
plot(lidar1, color = "Coplanar")


##### There no OSM Buildings Footprint found at this level!! No Progreess with this data
##### Same thing with Indiana3 
