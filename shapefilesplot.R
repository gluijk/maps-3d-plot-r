# Reading, processing and plotting spatial data from shapefiles
# www.overfitting.net

# Altimetría Comunidad de Madrid:
# https://datos.comunidad.madrid/catalogo/dataset/spacmaltimetria20m2000
# Curvas de nivel con equidistancia cada 20m para la Comunidad de Madrid

# ESRI shapefile format:
# Only three vector types: points, lines and polygons
# A shapefile consists of various files with same name but different extensions:
#  .shp (the geometry)
#  .dbf (the attributes)
#  .shx (the index that links the two)
#  .prj (the coordinate reference system)
# If the .prj file is missing a warning is given
# If any other file is missing an error occurs
# (one could in principle recover the .shx from the .shp file)

library(rgdal)  # readOGR()
library(viridis)  # perceptually uniform colourmaps

shp=readOGR(dsn="SIGI_MA_ALTIMETRIA_20Line.shp", verbose=T)

# Inspect shapefile
summary(shp)
names(shp)  # Attributes (=fields)
summary(shp@data)  # Attribute names
class(shp)  # SpatialLinesDataFrame
length(shp)  # Number of lines (curvas de nivel)
bbox(shp)  # X-Y bounding box
proj4string(shp)  # Projection

# Plot shapefile
# viridis:  opt = (A: magma; B: inferno; C: plasma; D: viridis; E: cividis)
pdf(file="shapefile_viridis.pdf")
spplot(shp, zcol=c('NM_COTA'), lwd=0.05, col.regions=viridis(20, opt="D"))
dev.off()


######################################
# Contour to DEM Raster conversion: 'raster contour shapefile in r'
# https://www.r-bloggers.com/2019/04/r-as-gis-for-ecologists/
# https://chitchatr.wordpress.com/2014/03/15/creating-dems-from-contour-lines-using-r/

library(raster)
library(rgdal)
library(rgeos)
library(gstat)
library(rgl)
library(rasterVis)

# Villapiedra
# UTM (Universal Transverse Mercator): North=4534178.01, East=432744.16, Zone=30T
# Lat = 40.955981 = 40°57'21.5"N / Long = -3.799163 = 3°47'57.0"W

# Coronales
# UTM (Universal Transverse Mercator): North=4479810.94, East=449653.12, Zone=30T
# Lat = 40.467449 = 40°28'02.8"N / Long = -3.593888 = 3°35'38.0"W

# Bola del mundo
# UTM (Universal Transverse Mercator): North=4515340.43, East=417352.19, Zone=30T
# Lat = 40.784896 = 40°47'05.6"N / Long = -3.979530 = 3°58'46.3"W

villa_x=432744.16
villa_y=4534178.01

villa_x=449653.12
villa_y=4479810.94

villa_x=417352.19
villa_y=4515340.43

test=data.frame(x=villa_x, y=villa_y)
coordinates(test)=~ x + y
proj4string(test)="+init=epsg:28992"

OFFSET=12000
shp_crop=crop(shp, extent(villa_x-OFFSET, villa_x+OFFSET*2,
                         villa_y-OFFSET, villa_y+OFFSET*2))
length(shp_crop)
spplot(shp_crop, zcol=c('NM_COTA'), lwd=0.1,
       sp.layout=list("sp.points", test, pch=13, cex=5, col="black"),
       col.regions=viridis(20, opt="D"))

# 1) Create a blank raster grid to interpolate the elevation data onto
dem_bbox=bbox(shp_crop)  # obtain extent
dem_rast=raster(nrows=200, ncols=200,  # create raster grid
                   xmn=dem_bbox[1, 1], 
                   xmx=ceiling(dem_bbox[1, 2]),
                   ymn=dem_bbox[2, 1], 
                   ymx=ceiling(dem_bbox[2, 2]))

projection(dem_rast)=CRS(projection(shp_crop))  # set projection
dim(dem_rast)
res(dem_rast)=20  # set resolution: the pixel size of the raster is defined: 5m x 5m pixel
dim(dem_rast)

# 2) Convert contour lines to points so we can interpolate between elevation points
dem_points=as(shp_crop, "SpatialPointsDataFrame")

# 3) Interpolate the elevation data onto the raster grid
dem_interp=gstat(formula=NM_COTA ~ 1, locations=dem_points,
                    set=list(idp=0), nmax=3)
# Obtain interpolation values for raster grid
DEM=interpolate(dem_rast, dem_interp)  # SLOW


######################################
# Plot DEM Raster

# Subset contour lines to 20m to enhance visualization
contour_plot=shp_crop[(shp_crop$NM_COTA) %in% 
                                   seq(min(shp_crop$NM_COTA), 
                                       max(shp_crop$NM_COTA), 
                                       100), ]                                                                   
# Plot 2D DEM with contour lines
pdf(file="contour_viridis.pdf")
plot(DEM, col=viridis(200, opt="D"))  # terrain.colors(20))
plot(contour_plot, add=T)
dev.off()

# Plot 3D DEM using rgl
plot3D(DEM, col=viridis(200, opt="C"), zfac=1)
plot3D(DEM, col=gray.colors(500, start=0.5, end=1), zfac=1)
plot3D(DEM, col=heat.colors, zfac=1)
plot3D(DEM, col=terrain.colors, zfac=0.5)
decorate3d(xlab='longitud', ylab='latitud', zlab='altitud')

