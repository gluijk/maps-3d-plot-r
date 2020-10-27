# Procesado de mapas vectoriales con R
# www.overfitting.net
# https://www.overfitting.net/2020/10/procesado-de-mapas-raster-y-vectoriales.html

library(rgdal)  # readOGR()
library(raster)  # crs(), spplot(), raster(), mask()
library(rasterVis)  # plot3D()
library(gstat)  # gstat()
library(viridis)  # perceptually uniform colourmaps


# INSPECCIÓN DEL FICHERO SHAPEFILE

# Altimetría Comunidad de Madrid:
# Curvas de nivel con equidistancia cada 20m para la Comunidad de Madrid
# URL: https://datos.comunidad.madrid/catalogo/dataset/spacmaltimetria20m2000

shp=readOGR(dsn="SIGI_MA_ALTIMETRIA_20Line.shp", verbose=T)
# ESRI shapefile format: only three vector types: points, lines and polygons
# A shapefile consists of various files with same name and tehse extensions:
#  .shp (the geometry)
#  .dbf (the attributes)
#  .shx (the index that links the two)
#  .prj (the coordinate reference system)
# If the .prj file is missing a warning is given
# If any other file is missing an error occurs
# (one could in principle recover the .shx from the .shp file)

# Inspect shapefile
summary(shp)
names(shp)  # attributes (=fields=variables)
summary(shp@data)  # attribute data summary
class(shp)  # SpatialLinesDataFrame
length(shp)  # 7239 features (=Lines=contours)
crs(shp)  # +proj=utm +zone=30 +ellps=GRS80 +towgs84= (...) +units=m +no_defs
proj4string(shp)  # projection (UTM)
bbox(shp)  # X-Y bounding box in UTM coordinates

# Conversión de líneas a puntos para conteo
dem_points=as(shp, "SpatialPointsDataFrame")
print(paste0(length(shp), " líneas (curvas de nivel) con ",
             length(dem_points), " puntos (",
             round(length(dem_points)/length(shp)), " puntos/línea)"))



# REPRESENTAMOS SUPERPOSICIÓN DE LÍNEAS Y PUNTOS

# Manzanares el Real (Plaza del Ayto.)
# UTM: North=4508778.29, East=426956.57, Zone=30T
# Lat = 40.726697 = 40°43'36.1"N / Long = -3.864947 = 3°51'53.8"W
site_x=426956.57
site_y=4508778.29
test=data.frame(x=site_x, y=site_y)
coordinates(test)=~ x + y
proj4string(test)="+init=epsg:28992"

OFFSET=1000  # +-1000m
shp_crop=crop(shp, extent(site_x-OFFSET, site_x+OFFSET,
                          site_y-OFFSET, site_y+OFFSET))
dem_points=as(shp_crop, "SpatialPointsDataFrame")

nbcol=100
color=viridis(nbcol, opt="D")
zcol=cut(shp_crop$NM_COTA, nbcol)

plot(shp_crop, xlab="UTM x (m)", ylab="UTM y (m)",
     main="SpatialLinesDataFrame + SpatialPointsDataFrame",
     axes=T, lwd=3, col=color[zcol])
plot(dem_points, col=rgb(0,0,0,alpha=0.15), pch=19, cex=1.7, add=T)
plot(test, pch=13, cex=5, lwd=2, col="red", add=T)



# DIBUJANDO CURVAS DE NIVEL Y MAPA 3D DE ELEVACIONES
# DESDE DATOS VECTORIALES

# Plot shapefile
# viridis(opt = A: magma; B: inferno; C: plasma; D: viridis; E: cividis)
spplot(shp, zcol=c('NM_COTA'), lwd=0.05,
       sp.layout=list("sp.points", test, pch=13, cex=5, lwd=2, col="red"),
       col.regions=viridis(200, opt="D"))

# Comparación con el Puig Campana
# 1406m = altitud Puig Campana
ALTMINMAD=min(shp$NM_COTA)
ALTMAXMAD=max(shp$NM_COTA)
COLS1MAD=round((1406-ALTMINMAD)/10)
COLS2MAD=round((ALTMAXMAD-1406)/10)
nbcol=COLS1MAD+COLS2MAD
pal=colorRampPalette(c("yellow", "orange", "red"))
color=c(gray.colors(COLS1MAD, start=0, end=1, gamma=2.2), pal(COLS2MAD))
spplot(shp, zcol=c('NM_COTA'), lwd=0.05,
       sp.layout=list("sp.points", test, pch=13, cex=5, lwd=2, col="red"),
       col.regions=color)


# Contour to DEM Raster conversion:
# https://www.r-bloggers.com/2019/04/r-as-gis-for-ecologists/
# https://chitchatr.wordpress.com/2014/03/15/creating-dems-from-contour-lines-using-r/

# Hacemos zoom en Manzanares el Real
OFFSET=9100  # +-9100m
shp_crop=crop(shp, extent(site_x-OFFSET, site_x+OFFSET,
                          site_y-OFFSET, site_y+OFFSET))
# DELTA_COTA=100  # simplificamos el mapa
# shp_crop=subset(shp_crop, NM_COTA %% DELTA_COTA==0)
length(shp_crop)

spplot(shp_crop, zcol=c('NM_COTA'), lwd=0.05,
       sp.layout=list("sp.points", test, pch=13, cex=5, lwd=2, col="red"),
       col.regions=viridis(200, opt="D"))

# Interpolamos raster a partir de los datos vectoriales
# 1) Create a blank raster grid to interpolate the elevation data onto
dem_bbox=bbox(shp_crop)  # obtain extent
ANCHO_INTERP=200  # no se usa este valor
dem_rast=raster(nrows=ANCHO_INTERP, ncols=ANCHO_INTERP,  # create raster grid
                xmn=dem_bbox[1, 1], 
                xmx=ceiling(dem_bbox[1, 2]),
                ymn=dem_bbox[2, 1], 
                ymx=ceiling(dem_bbox[2, 2]))

projection(dem_rast)=CRS(projection(shp_crop))  # set projection
dim(dem_rast)
res(dem_rast)=5  # set resolution: pixel size of the raster is defined: 5m x 5m
dim(dem_rast)  # raster size has changed

# 2) Convert contour lines to points to interpolate between elevation points
dem_points=as(shp_crop, "SpatialPointsDataFrame")

# 3) Interpolate the elevation data onto the raster grid
dem_interp=gstat(formula=NM_COTA ~ 1, locations=dem_points,
                    set=list(idp=0), nmax=3)
# Obtain interpolation values for raster grid
DEM=interpolate(dem_rast, dem_interp)  # can take time... (5min)


# Dibujamos el raster interpolado en 2D (curvas) y 3D (elevaciones):
                                                                 
# Plot 2D DEM
plot(DEM, col=viridis(200, opt="D"), axes=F, box=F, legend=F)
plot(test, pch=13, cex=5, lwd=2, col="red", add=T)
# Add original contour lines
contour_plot=shp_crop[(shp_crop$NM_COTA) %in% 
                          seq(min(shp_crop$NM_COTA), 
                              max(shp_crop$NM_COTA), 
                              20), ]  # 20m contour lines
plot(contour_plot, add=T, lwd=0.1)


# Plot 3D DEM using rgl
plot3D(DEM, col=viridis(200, opt="D"), zfac=1)



# CÁLCULO DISTRIBUCIÓN DE ALTITUDES VÍA RASTERIZACIÓN

MINHIST=min(shp$NM_COTA)
MAXHIST=max(shp$NM_COTA)

# Interpolamos raster a partir de los datos vectoriales
# 1) Create a blank raster grid to interpolate the elevation data onto
dem_bbox=bbox(shp)  # obtain extent
ANCHO_INTERP=2000
dem_rast=raster(nrows=ANCHO_INTERP, ncols=round(ANCHO_INTERP*
                    (dem_bbox[1, 2]-dem_bbox[1, 1])/
                    (dem_bbox[2, 2]-dem_bbox[2, 1])),  # create raster grid
                xmn=dem_bbox[1, 1], 
                xmx=ceiling(dem_bbox[1, 2]),
                ymn=dem_bbox[2, 1], 
                ymx=ceiling(dem_bbox[2, 2]))

projection(dem_rast)=CRS(projection(shp))  # set projection
dim(dem_rast)

# 2) Convert contour lines to points to interpolate between elevation points
dem_points=as(shp, "SpatialPointsDataFrame")

# 3) Interpolate the elevation data onto the raster grid
dem_interp=gstat(formula=NM_COTA ~ 1, locations=dem_points,
                 set=list(idp=0), nmax=3)
# Obtain interpolation values for raster grid
DEM=interpolate(dem_rast, dem_interp)  # can take time... (25min!)

# Shapefile con los polígonos de las 'Líneas límite municipales'
# URL: http://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=CAANE
prov=readOGR(dsn="recintos_provinciales_inspire_peninbal_etrs89.shp", verbose=T)
mad=subset(prov, NAMEUNIT=='Madrid')  # just Madrid

# Dibujar Madrid en polígonos
plot(mad, axes=T, xlab="Longitude", ylab="Latitude")  # Lat/Long
mad=spTransform(mad, crs(DEM))  # match CRS
plot(mad, axes=T, xlab="UTM x (m)", ylab="UTM y (m)")  # coords. UTM
plot(test, pch=13, cex=5, lwd=2, col="red", add=T)

corte=mask(DEM, mad)  # del raster interpolado estrictamente mantenemos Madrid
writeRaster(corte, "interpolacionmask.tif", overwrite=T)

plot(DEM, col=c(gray.colors(100, start=0, end=1, gamma=2.2)),
     axes=F, box=F, legend=F)
plot(corte, col=c(gray.colors(100, start=0, end=1, gamma=2.2)),
     axes=F, box=F, legend=F)

v=getValues(corte)  # vector with values
v=v[is.na(v)==F]
hist(v,
     breaks=100, xlim=c(MINHIST, MAXHIST),
     main=paste0("Distr. altitudes Comunidad de Madrid (",
                 length(v)," celdas interpoladas)"),
     xlab=paste0("min / mediana / media / max = ",
                 round(min(v)), "m / ",
                 round(median(v)), "m / ",
                 round(mean(v)), "m / ",
                 round(max(v)), "m"))
abline(v=median(v), col='red', lty='dashed', lwd=2)
abline(v=mean(v), col='red', lty='dashed', lwd=2)


# Madrid en 3D
plot3D(corte, col=viridis(200, opt="D"), zfac=0.5)
