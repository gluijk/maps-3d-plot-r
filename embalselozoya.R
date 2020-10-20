# Procesado de mapas raster y vectoriales con R (EMBALSE LOZOYA)
# www.overfitting.net
# https://www.overfitting.net/2020/10/procesado-de-mapas-raster-y-vectoriales.html



# DIBUJANDO MAPA 3D DE ELEVACIONES DESDE DATOS RASTER

# Centro de Descargas del Centro Nacional de Información Geográfica
# Modelos de elevaciones en formato raster MDT25 (resolución rejilla=25m)
# URL: http://centrodedescargas.cnig.es/CentroDescargas/index.jsp

library(data.table)  # fread()
library(tiff)
library(png)
library(rgl)  # persp3d()


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

dim(sierra)  # 1486 x 2255

ALTO=nrow(sierra)
ANCHO=ncol(sierra)
# Zoom en el área del Valle del Lozoya
sierra=sierra[1:(ALTO*2/3+200), (ANCHO*1/3-200):(ANCHO*1/3+ALTO*2/3-1)]
dim(sierra)  # 1190 x 1190
sierraBK=sierra

RESOLUCION=25  # resolución rejilla=25m
ALTO=nrow(sierra)
ANCHO=ncol(sierra)
ALTO_m=ALTO*RESOLUCION
ANCHO_m=ANCHO*RESOLUCION
ALTMIN_m=min(sierra)
ALTMAX_m=max(sierra)
fZ=2  # factor relativo en altitud


# Guardamos raster en TIFF
writeTIFF(sierra/ALTMAX_m, "sierra.tif", bits.per.sample=16, compression="LZW")

# Mapa 3D de elevaciones
ALTAGUA=1450
COLS1=round((ALTAGUA-ALTMIN_m)/1)
COLS2=round((ALTMAX_m-ALTAGUA)/1)
nbcol=COLS1+COLS2
pal=colorRampPalette(c("yellow", "orange", "red"))
color=c(gray.colors(COLS1, start=0, end=1, gamma=2.2), pal(COLS2))


N=100
WIDTH=1600  # 1280  # 800  # 512  # Resolución de salida
HEIGHT=1000  # 800  # 960  # 600  # 384

# Rango inundación: 1060 -> 1450
INUNDAMIN=1060
INUNDAMAX=1450
SCALE=INUNDAMAX-INUNDAMIN  # rango cotas

ref=readPNG("embalselozoyaVACIO.png")  # imagen inicial sin agua
for (f in 0:(N-1)) {
    h=SCALE*(1-cos(2*pi*f/N))/2+INUNDAMIN
    sierra=sierraBK
    indices=which(
        row(sierra)<ALTO-400 &
        col(sierra)>336 &
        col(sierra)<(264-1084)/1190*row(sierra)+1084 &
        col(sierra)<(1190-900)/614*row(sierra)+900 &
        sierra<h)
    sierra[indices]=h
    
    # Mapa 3D de elevaciones
    color=c(gray.colors(COLS1, start=0, end=1, gamma=2.2), pal(COLS2))
    POS=round((h-ALTMIN_m)/1)
    color[c(POS-1, POS, POS+1)]="#66EEFF"  # color del agua

    zcol=cut(sierra, nbcol)
    persp3d(z=sierra, col=color[zcol], xlab="", ylab="", zlab="",
            aspect=c(ALTO_m, ANCHO_m, ALTMAX_m*fZ), axes=F, box=F)
    bg3d(color="white")

    view3d(theta=0, phi=-60)  # ajuste de phi
    um=par3d()$userMatrix
    um=rotate3d(um, angle=-pi-pi/8, x=0, y=0, z=1)
    par3d(FOV=40, zoom=0.6, userMatrix=um,
          windowRect=c(100, 100, WIDTH+100, HEIGHT+100))
    
    nombre=paste0("embalselozoya_",
                  ifelse(f<10, "00", ifelse(f<100, "0", "")), f, ".png")
    snapshot3d(nombre, fmt="png", top=T)
    rgl.close()
    
    frame=readPNG(nombre)*0.7+ref*0.3  # agua transparente
    frame=frame[201:HEIGHT, 1:1450,]  # recorte
    writePNG(image=frame, target=nombre)
    
    print(paste0("Frame ", f+1, "/", N, " - h=", h))
}

# Parámetros del embalse
# (Ref.: embalse de El Atazar: 426hm3)

# Estimación capacidad del embalse en m3
# La separamos del bucle anterior por precisión y sencillez
nivel=c()
volm3=c()
for (h in INUNDAMIN:INUNDAMAX) {  # recorremos secciones de 1m de alto
    sierra=sierraBK
    indices=which(row(sierra)<ALTO-400 &
                      col(sierra)>336 &
                      col(sierra)<(264-1084)/1190*row(sierra)+1084 &
                      col(sierra)<(1190-900)/614*row(sierra)+900 &
                      sierra<h)
    nivel=c(nivel, h)
    volm3=c(volm3, length(indices)*RESOLUCION^2)  # m3 almacenados en h
}


plot(nivel, volm3/1000000, type='l', col='red',
     main='Capacidad por cota de altitud',
     ylab='hm3', xlab='Cota (m)')
plot(nivel-nivel[1], cumsum(volm3/1000000), type='l', col='red',
     main='Capacidad acumulada por nivel de llenado',
     ylab='hm3', xlab='Nivel de llenado (m)')

NUM=length(nivel)
print(paste0("Capacidad máx. del embalse: ", round(sum(volm3)/1000000), "hm3"))
print(paste0("Superficie máx. del embalse: ", round(volm3[NUM]/1000000), "km2"))
print(paste0("Altura máx. de la presa: ", nivel[NUM]-nivel[1], "m"))
print(paste0("Longitud de la presa grande: ",
             round((104^2+151^2)^0.5*RESOLUCION/1000, digits=1), "km"))


# Mapa raster 2D
image(t(sierra[nrow(sierra):1,])-min(sierra),
      col=c(c(gray.colors(COLS1, start=0, end=1, gamma=2.2), pal(COLS2))),
      #col=color,
      useRaster=T, asp=ALTO/ANCHO, axes=F)
