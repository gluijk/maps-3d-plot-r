# Procesado de mapas raster con R
# www.overfitting.net
# https://www.overfitting.net/2020/10/procesado-de-mapas-raster-con-r.html

library(data.table)  # fread()
library(tiff)
library(rgl)  # persp3d()


# DIBUJANDO MAPA 3D DE ELEVACIONES DESDE DATOS RASTER

# Centro de Descargas del Centro Nacional de Información Geográfica
# Modelos de elevaciones en formato raster MDT25 (resolución rejilla=25m)
# URL: http://centrodedescargas.cnig.es/CentroDescargas/index.jsp

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

RESOLUCION=25
ALTO=nrow(sierra)
ANCHO=ncol(sierra)
ALTO_m=ALTO*RESOLUCION
ANCHO_m=ANCHO*RESOLUCION
ALTMIN_m=min(sierra)
ALTMAX_m=max(sierra)
f=3  # factor relativo en altitud

# Guardamos raster en TIFF
writeTIFF(sierra/ALTMAX_m, "sierra.tif", bits.per.sample=16, compression="LZW")


# Mapa 3D de elevaciones
# 1406m = altitud Puig Campana
COLS1=round((1406-ALTMIN_m)/10)
COLS2=round((ALTMAX_m-1406)/10)
nbcol=COLS1+COLS2
pal=colorRampPalette(c("yellow", "orange", "red"))
color=c(gray.colors(COLS1, start=0, end=1, gamma=2.2), pal(COLS2))
zcol=cut(sierra, nbcol)
persp3d(z=sierra, col=color[zcol], xlab="", ylab="", zlab="",
        aspect=c(ALTO_m, ANCHO_m, ALTMAX_m*f), axes=F, box=F)


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
        aspect=c(ALTO_m, ANCHO_m, ALTMAX_m*f), axes=F, box=F)

clear3d("lights")
light3d(x=10, y=0, z=10, viewpoint.rel=F, diffuse = "#FFFFFF")
light3d(x=-10, y=0, z=10, viewpoint.rel=F, diffuse = "#FFFFFF")
rgl.light(x=0, y=0, z=150, viewpoint.rel=T, ambient="#000000",
          diffuse="#FFFFFF", specular="#000000")
view3d(theta=0, phi=0)  # visión cenital
um=par3d()$userMatrix
um=rotate3d(um, angle=-pi/2, x=0, y=0, z=1)
par3d(FOV=0, zoom=1, userMatrix=um)  # axonométrica


# Opción 2: mapa 2D
nbcol=100
pal=colorRampPalette(c(rgb(0,1,0), rgb(0,1,0), rgb(0,1,0),
                       rgb(0.5,0.5,0.5),
                       rgb(1,0,0), rgb(1,0,0), rgb(1,0,0)))
image(t(pend_ns[nrow(pend_ns):1,]), col=pal(nbcol), useRaster=T,
      asp=nrow(pend_ns)/ncol(pend_ns), axes=F)



# TENERIFE

teide=readTIFF("tenerifecomposite.tif")  # 2710x3243
teide[teide==0]=NaN  # mar

RESOLUCION=25
ALTO=nrow(teide)
ANCHO=ncol(teide)
ALTO_m=ALTO*RESOLUCION
ANCHO_m=ANCHO*RESOLUCION
ALTTEIDE_m=3718
f=3  # factor relativo en altitud


# Mapa 3D de elevaciones
# 1406m = altitud Puig Campana
COLS1=round((1406-0)/10)
COLS2=round((ALTTEIDE_m-1406)/10)
nbcol=COLS1+COLS2
pal=colorRampPalette(c("yellow", "orange", "red"))
color=c(gray.colors(COLS1, start=0, end=1, gamma=2.2), pal(COLS2))
zcol=cut(teide, nbcol)
persp3d(z=teide, col=color[zcol], xlab="", ylab="", zlab="",
        aspect=c(ALTO_m, ANCHO_m, ALTTEIDE_m*f), axes=F, box=F)


# Distribución de altitudes
v=teide[is.na(teide)==F]*ALTTEIDE_m
hist(v,
     breaks=100, xlim=c(0, ALTTEIDE_m),
     main=paste0("Distr. altitudes Tenerife (",
                 length(v)," celdas interpoladas)"),
     xlab=paste0("min / mediana / media / max = ",
                 round(min(v)), "m / ",
                 round(median(v)), "m / ",
                 round(mean(v)), "m / ",
                 round(max(v)), "m"))
abline(v=median(v), col='red', lty='dashed', lwd=2)
abline(v=mean(v), col='red', lty='dashed', lwd=2)

