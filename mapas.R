# Procesado de mapas raster y vectoriales con R
# www.overfitting.net
# https://www.overfitting.net/2020/10/procesado-de-mapas-raster-y-vectoriales.html



# DIBUJANDO MAPA 3D DE ELEVACIONES DESDE DATOS RASTER

# Centro de Descargas del Centro Nacional de Información Geográfica
# Modelos de elevaciones en formato raster MDT25 (resolución rejilla=25m)
# URL: http://centrodedescargas.cnig.es/CentroDescargas/index.jsp

library(data.table)  # fread()
library(tiff)
library(rgl)  # persp3d()
library(raster)  # spplot(), raster(), mask()


# Leemos y procesamos datos raster
# 4 cuadrantes Sierra Norte de Madrid (Sierra de Guadarrama, Valle del Lozoya)
# Cotas en m, resolución rejilla=25m
sierra_11=data.matrix(
    fread("PNOA_MDT25_ETRS89_HU30_0483_LID.txt", sep=" ", dec="."))
sierra_12=data.matrix(
    fread("PNOA_MDT25_ETRS89_HU30_0484_LID.txt", sep=" ", dec="."))
sierra_21=data.matrix(
    fread("PNOA_MDT25_ETRS89_HU30_0508_LID.txt", sep=" ", dec="."))
sierra_22=data.matrix(
    fread("PNOA_MDT25_ETRS89_HU30_0509_LID.txt", sep=" ", dec="."))

# Eliminar solapes y crop final (valores obtenidos manualmente)
sierra=matrix(0, nrow=1508, ncol=2269)
sierra[1:759, 11:1148]=sierra_11
sierra[14:768, 1136:2269]=sierra_12
sierra[741:1499, 1:1141]=sierra_21
sierra[754:1508, 1129:2265]=sierra_22
sierra=sierra[14:1499, 11:2265]
rm(sierra_11, sierra_12, sierra_21, sierra_22)

dim(sierra)  # -> 1486 x 2255
ALTMIN=min(sierra)
ALTMAX=max(sierra)
ALTO=nrow(sierra)
ANCHO=ncol(sierra)
writeTIFF(((sierra-ALTMIN)/(ALTMAX-ALTMIN))**(1/2.2),
          "sierra.tif", bits.per.sample=16, compression="LZW")


# Mapa 3D de elevaciones
# 1406m = altitud Puig Campana
COLS1=round((1406-ALTMIN)/10)
COLS2=round((ALTMAX-1406)/10)
nbcol=COLS1+COLS2
pal=colorRampPalette(c("yellow", "orange", "red"))
color=c(gray.colors(COLS1, start=0, end=1, gamma=2.2), pal(COLS2))
zcol=cut(sierra, nbcol)
persp3d(z=sierra, col=color[zcol], xlab="", ylab="", zlab="",
        aspect=c(1, ANCHO/ALTO, 0.15), axes=F, box=F)
bg3d(color="black")


# Mapa de insolación N-S
d=25  # cell size=25m
i=which(row(sierra)>1 & row(sierra)<ALTO)  # skip first and last row
pend_ns=sierra*0
pend_ns[i]=(sierra[i+1]-sierra[i-1])/(2*d)  # calculate N-S slope
pend_ns[1,]=pend_ns[2,]  # replicate first and...
pend_ns[ALTO,]=pend_ns[ALTO-1,]  # ...last row

# Simetrizamos alrededor de pendiente=0
PENDMAX=min(abs(min(pend_ns)),max(pend_ns))
pend_ns[pend_ns < -PENDMAX]=-PENDMAX
pend_ns[pend_ns > PENDMAX]=PENDMAX


# Opción 1: mapa 3D en visión axonométrica
# NOTA: sierra y pend_ns deben tener las mismas dimensiones
# para poder usar el primero como elevación y el segundo como color
nbcol=100
pal=colorRampPalette(c(rgb(0.8,0.6,0), rgb(0.8,0.6,0), rgb(0.8,0.6,0),
                       rgb(0.5,0.5,0.5),
                       rgb(0.1,0.5,0.7), rgb(0.1,0.5,0.7), rgb(0.1,0.5,0.7)))
color=pal(nbcol)
zcol=cut(pend_ns, nbcol)
persp3d(z=sierra, col=color[zcol], xlab="", ylab="", zlab="",
        aspect=c(1, ANCHO/ALTO, 0.2), axes=F, box=F)

clear3d("lights")
light3d(x=10, y=0, z=10, viewpoint.rel=F, diffuse = "#FFFFFF")
light3d(x=-10, y=0, z=10, viewpoint.rel=F, diffuse = "#FFFFFF")
rgl.light(x=0, y=0, z=150, viewpoint.rel=T, ambient="#000000",
          diffuse="#FFFFFF", specular="#000000")
view3d(theta=0, phi=0)  # visión cenital
um=par3d()$userMatrix
um=rotate3d(um, -pi/2, 0, 0, 1)
par3d(FOV=0, zoom=1, userMatrix=um)  # axonométrica


# Opción 2: mapa 2D
nbcol=100
pal=colorRampPalette(c(rgb(0,1,0), rgb(0,1,0), rgb(0,1,0),
                       rgb(0.5,0.5,0.5),
                       rgb(1,0,0), rgb(1,0,0), rgb(1,0,0)))
image(t(pend_ns[nrow(pend_ns):1,]), col=pal(nbcol), useRaster=T,
      asp=nrow(pend_ns)/ncol(pend_ns), axes=F)



# DIBUJANDO CURVAS DE NIVEL Y MAPA 3D DE ELEVACIONES
# DESDE DATOS VECTORIALES (FORMATO SHAPEFILE)

# Altimetría Comunidad de Madrid:
# Curvas de nivel con equidistancia cada 20m para la Comunidad de Madrid
# URL: https://datos.comunidad.madrid/catalogo/dataset/spacmaltimetria20m2000

library(rgdal)  # readOGR()
library(rasterVis)  # plot3D()
library(gstat)  # gstat()
library(viridis)  # perceptually uniform colourmaps
# library(rgeos)  # gSimplify()


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
length(shp)  # 7239 features (contours)
crs(shp)  # +proj=utm +zone=30 +ellps=GRS80 +towgs84= (...) +units=m +no_defs
proj4string(shp)  # projection (UTM)
bbox(shp)  # X-Y bounding box in UTM coordinates


# Manzanares el Real (Plaza del Ayto.)
# UTM: North=4508778.29, East=426956.57, Zone=30T
# Lat = 40.726697 = 40°43'36.1"N / Long = -3.864947 = 3°51'53.8"W
site_x=426956.57
site_y=4508778.29
test=data.frame(x=site_x, y=site_y)
coordinates(test)=~ x + y
proj4string(test)="+init=epsg:28992"

# Plot shapefile
# viridis:  opt = A: magma; B: inferno; C: plasma; D: viridis; E: 
spplot(shp, zcol=c('NM_COTA'), lwd=0.05,
       sp.layout=list("sp.points", test, pch=13, cex=5, lwd=2, col="red"),
       col.regions=viridis(20, opt="D"))

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
OFFSET=9100
shp_crop=crop(shp, extent(site_x-OFFSET, site_x+OFFSET,
                          site_y-OFFSET, site_y+OFFSET))
# DELTA_COTA=100  # simplificamos el mapa
# shp_crop=subset(shp_crop, NM_COTA %% DELTA_COTA==0)
length(shp_crop)

spplot(shp_crop, zcol=c('NM_COTA'), lwd=0.05,
       sp.layout=list("sp.points", test, pch=13, cex=5, lwd=2, col="red"),
       col.regions=viridis(20, opt="D"))

# Interpolamos raster a partir de los datos vectoriales
# 1) Create a blank raster grid to interpolate the elevation data onto
dem_bbox=bbox(shp_crop)  # obtain extent
ANCHO_INTERP=200
dem_rast=raster(nrows=ANCHO_INTERP, ncols=ANCHO_INTERP,  # create raster grid
                xmn=dem_bbox[1, 1], 
                xmx=ceiling(dem_bbox[1, 2]),
                ymn=dem_bbox[2, 1], 
                ymx=ceiling(dem_bbox[2, 2]))

projection(dem_rast)=CRS(projection(shp_crop))  # set projection
dim(dem_rast)
res(dem_rast)=5  # set resolution: the pixel size of the raster is defined: 5m x 5m pixel
dim(dem_rast)

# 2) Convert contour lines to points so we can interpolate between elevation points
dem_points=as(shp_crop, "SpatialPointsDataFrame")

# 3) Interpolate the elevation data onto the raster grid
dem_interp=gstat(formula=NM_COTA ~ 1, locations=dem_points,
                    set=list(idp=0), nmax=3)
# Obtain interpolation values for raster grid
DEM=interpolate(dem_rast, dem_interp)  # can take time...


# Dibujamos el raster interpolado en 2D (curvas) y 3D (elevaciones):
                                                                 
# Plot 2D DEM adding original contour lines
plot(DEM, col=viridis(200, opt="D"))  # terrain.colors(20))
contour_plot=shp_crop[(shp_crop$NM_COTA) %in% 
                          seq(min(shp_crop$NM_COTA), 
                              max(shp_crop$NM_COTA), 
                              20), ]  # 20m contour lines
plot(contour_plot, add=T, lwd=0.1)

# Plot 3D DEM using rgl
plot3D(DEM, col=viridis(200, opt="D"), zfac=1)



# CÁLCULO DISTRIBUCIÓN DE ALTITUDES

MINHIST=min(shp$NM_COTA)
MAXHIST=max(shp$NM_COTA)

# Distr. altitudes Comunidad de Madrid - CON 8141 INTERPOLACIONES RASTER

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

# 2) Convert contour lines to points so we can interpolate between elevation points
dem_points=as(shp, "SpatialPointsDataFrame")

# 3) Interpolate the elevation data onto the raster grid
dem_interp=gstat(formula=NM_COTA ~ 1, locations=dem_points,
                 set=list(idp=0), nmax=3)
# Obtain interpolation values for raster grid
DEM=interpolate(dem_rast, dem_interp)  # can take time...

# Shapefile con los polígonos de las 'Líneas límite municipales'
# URL: http://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=CAANE
prov=readOGR(dsn="recintos_provinciales_inspire_peninbal_etrs89.shp", verbose=T)
mad=subset(prov, NAMEUNIT=='Madrid')  # just Madrid
plot(mad, axes=T)
mad=spTransform(mad, crs(DEM))  # match CRS
plot(mad, axes=T)
corte=mask(DEM, mad)  # del raster interpolado estrictamente mantenemos Madrid
writeRaster(corte, "interpolacionmask.tif", overwrite=T)

plot(DEM, col=c(gray.colors(COLS1, start=0, end=1, gamma=2.2)),
     axes=F, box=F, legend=F)
plot(corte, col=c(gray.colors(COLS1, start=0, end=1, gamma=2.2)),
     axes=F, box=F, legend=F)
plot3D(corte, col=viridis(200, opt="D"), zfac=0.5)

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



# TENERIFE

teide=readTIFF("tenerifecomposite.tif")  # 2710x3243
teide[teide==0]=NaN  # mar

RESOLUCION=25
ALTO=nrow(teide)
ANCHO=ncol(teide)
ANCHO_m=ANCHO*RESOLUCION
ALTO_m=ALTO*RESOLUCION
ALTTEIDE_m=3718
f=3  # factor relativo en altitud


# Mapa 3D de elevaciones
# 1406m = altitud Puig Campana
COLS1=round((1406-ALTMIN)/10)
COLS2=round((ALTMAX-1406)/10)
nbcol=COLS1+COLS2
pal=colorRampPalette(c("yellow", "orange", "red"))
color=c(gray.colors(COLS1, start=0, end=1, gamma=2.2), pal(COLS2))
zcol=cut(teide, nbcol)
persp3d(z=teide, col=color[zcol], xlab="", ylab="", zlab="",
        aspect=c(ALTO_m, ANCHO_m, ALTTEIDE_m*f), axes=F, box=F)
bg3d(color="white")


# Distribución de altitudes
v=teide[is.na(teide)==F]*ALTTEIDE_m
hist(v,
     breaks=100, xlim=c(0, ALTTEIDE_m),
     main=paste0("Distr. altitudes Tenerife (",
                 length(v)," celdas promediadas)"),
     xlab=paste0("min / mediana / media / max = ",
                 round(min(v)), "m / ",
                 round(median(v)), "m / ",
                 round(mean(v)), "m / ",
                 round(max(v)), "m"))
abline(v=median(v), col='red', lty='dashed', lwd=2)
abline(v=mean(v), col='red', lty='dashed', lwd=2)

