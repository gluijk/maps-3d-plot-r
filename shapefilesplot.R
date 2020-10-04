# Paquetes rgdal y sp
# www.overfitting.net

# Altimetría Comunidad de Madrid:
# https://datos.comunidad.madrid/catalogo/dataset/spacmaltimetria20m2000
# Mapa que recoge las curvas de nivel, con equidistancia cada 20m para
# el territorio de la Comunidad de Madrid. Se generó a partir de la cartografía
# topográfica elaborada por el entonces Servicio Cartográfico Regional,
# actualmente Centro Regional de Información Cartográfica.

setwd("D:/R/41_CurvasLozoya")
library(viridis)

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
library(sp)
shp=readOGR(dsn="SIGI_MA_ALTIMETRIA_20Line.shp", verbose=T)
# shp=readOGR(dsn=getwd(), layer="SIGI_MA_ALTIMETRIA_20Line", verbose=T)

# Basic plot of this shape file
plot(shp)
par(mar=c(0,0,0,0))  # number of lines of margin on the four sides of the plot
plot(shp, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0)


# Shapefile object
shp
summary(shp)
names(shp)  # Attributes (=fields)
summary(shp@data)
class(shp)  # SpatialLinesDataFrame
length(shp)  # Number of lines (curvas de nivel)
bbox(shp)  # Bounding box
proj4string(shp)  # Projection

# Plot
# viridis
png(file="salida_puig_magma_ALL.png", bg="transparent", width=800, height=800)
spplot(subset(shp, NM_COTA>0), zcol=c('NM_COTA'),
       xlim=bbox(shp)[1, ],
       ylim=bbox(shp)[2, ],
       col.regions=viridis(20, opt="A")
       )
dev.off()

# gray
png(file="salida_puig_gray.png", width=800, height=800)
spplot(shp, zcol=c('NM_COTA'),
       xlim=bbox(shp)[1, ],
       ylim=bbox(shp)[2, ],
       col.regions=c('gray')
       )
dev.off()


# Villapiedra
# Lat = 40.955981 = 3°47'57.0"W / Long = -3.799163 = 40°57'21.5"N
# UTM (Universal Transverse Mercator):
# East=432744.16, North=4534178.01, Zone=30T
villa_x=432744.16
villa_y=4534178.01
test=data.frame(x=villa_x, y=villa_y)
coordinates(test)=~ x + y
proj4string(test)="+init=epsg:28992"

OFFSET=25000
pdf(file="villapiedra.pdf")
spplot(shp, zcol=c('NM_COTA'), lwd=0.05,
       xlim=c(villa_x-OFFSET, villa_x+OFFSET),
       ylim=c(villa_y-OFFSET, villa_y+OFFSET),
       sp.layout=list("sp.points", test, pch=13, cex=2, col="black"))
dev.off()

# viridis:  (A: magma; B: inferno; C: plasma; D: viridis; E: cividis)
pdf(file="villapiedra_rainbow.pdf")
spplot(shp, zcol=c('NM_COTA'), lwd=0.05,
       xlim=bbox(shp)[1, ],
       ylim=bbox(shp)[2, ],
       # col.regions=rainbow(20),
       col.regions=viridis(20, opt="D"),
       sp.layout=list("sp.points", test, pch=13, cex=2, col="black")
       )
dev.off()



######################################
# Aislamos una línea (4 tramos con 5 vertices)

shp_sub=subset(shp, NM_COTA==2420)
spplot(shp_sub, zcol=c('NM_COTA'), col.regions=viridis(20, opt="A"))



######################################
# shp in ggplot2

# 'fortify' the data to get a dataframe format required by ggplot2
library(broom)
spdf_fortified <- broom::tidy(shp, region="NM_COTA")

# Plot it
library(ggplot2)
ggplot() + geom_polygon(data=spdf_fortified, aes(x=long, y=lat, group=group),
                 fill="#69b3a2", color="white") + theme_void()

map <- ggplot() + geom_polygon(data=shp, aes(x=long, y=lat, group=group),
                               colour = "black", fill = NA)
map + theme_void()

shp@data <- shp@data %>% mutate(id = row.names(.))
shp_df <- broom::tidy(shp, region = "id")
shp_df <- shp_df %>% left_join(shp@data, by = c("id"="id"))
map <- ggplot() + geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = isKingdom), colour = "black") + theme_void()
map 