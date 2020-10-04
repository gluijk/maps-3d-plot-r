# Reading, processing and plotting spatial data from shapefiles
# www.overfitting.net

# Altimetría Comunidad de Madrid:
# https://datos.comunidad.madrid/catalogo/dataset/spacmaltimetria20m2000
# Mapa que recoge las curvas de nivel, con equidistancia cada 20m para
# el territorio de la Comunidad de Madrid. Se generó a partir de la cartografía
# topográfica elaborada por el entonces Servicio Cartográfico Regional,
# actualmente Centro Regional de Información Cartográfica.

# Introduction to Spatial Data Types in R
# https://cengel.github.io/rspatial/2_spDataTypes.nb.html

# Open and Plot Shapefiles in R (shp en ggplot2)
# https://www.r-graph-gallery.com/168-load-a-shape-file-into-r.html
# https://rpubs.com/huanfaChen/ggplotShapefile

# A Step-by-Step Guide to Making 3D Maps with Satellite Imagery in R
# https://www.tylermw.com/a-step-by-step-guide-to-making-3d-maps-with-satellite-imagery-in-r/

# Reading and writing of "ESRI shapefile" format spatial data.
# Only the three vector types (points, lines, and polygons) can be stored in shapefiles.
# These are simple wrapper functions around readOGR and writeOGR (rgdal package).
# A shapefile consists of various files of the same name but different extensions:
#  .shp (the geometry)
#  .dbf (the attributes)
#  .shx (the index that links the two)
#  .prj (the coordinate reference system)
# If the .prj file is missing a warning is given.
# If any other file is missing an error occurs.
# (although one could in principle recover the .shx from the .shp file).
# Additional files are ignored.
                                                                                                                                                                                                                                                                                                                                        
library(rgdal)  # readOGR()
library(viridis)  # perceptually uniform colourmaps

shp=readOGR(dsn="SIGI_MA_ALTIMETRIA_20Line.shp", verbose=T)

# Inspect shapefile object
summary(shp)
names(shp)  # Attributes (=fields)
summary(shp@data)  # Attribute names
class(shp)  # SpatialLinesDataFrame
length(shp)  # Number of lines (curvas de nivel)
bbox(shp)  # X-Y bounding box
proj4string(shp)  # Projection


# Plot shapefile object
# viridis:  opt = (A: magma; B: inferno; C: plasma; D: viridis; E: cividis)
pdf(file="shapefile_viridis.pdf")
spplot(shp, zcol=c('NM_COTA'), lwd=0.05, col.regions=viridis(20, opt="D"))
dev.off()



######################################
# Plot shapefile object with ggplot2

# 'fortify' the data to get a dataframe format required by ggplot2
library(broom)
spdf_fortified <- broom::tidy(shp, region="NM_COTA")

# Plot it
library(ggplot2)
ggplot() + geom_polygon(data=spdf_fortified, aes(x=long, y=lat, group=group),
                 fill="#69b3a2", color="white") + theme_void()
