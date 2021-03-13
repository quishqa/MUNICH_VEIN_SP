# Plotting street links for each MUNICH domain
# with building height retrieve from WUDAPT LCZ 
# Based in Pellegati et al. (2019)

library(raster)
library(rgdal)
library(plyr)
library(viridis)
library(rasterVis)

wu_geo <- raster("02_data/wudapt_lat_lon.tif")

# Building height for each LCZ
lcz <- data.frame(lcz = c(1:10, c(101:107)), 
                  bh  = c(45, 15, 5, 40, 15, 5, 3, 7, 5, 8.5, 
                          20, 7.5, 1.5, 1, 0.05, 0.1, 0.05),
                  name = paste0("LCZ", c(1:10, LETTERS[1:7])))

# Transforming raster to factor
wp <- as.factor(wu_geo)
rat <- levels(wp)
rat[['LCZ']] <- lcz$name
levels(wp) <- rat

levelplot(wp)

# Pinheiros domain --------------------------------------------------------

# Loading Pinheiros neighborhood
pin_streets <-  read.table('02_data/emiss/PINUGS.traf.2014100800',
                           header = T)

# Retrieving link start and end coordinates
pin_streets <- pin_streets[, c('xa', 'xb', 'ya', 'yb')]
utm.a <- SpatialPoints(cbind(pin_streets$xa, pin_streets$ya), proj4string = CRS("+proj=utm +zone=23 +south"))
utm.b <- SpatialPoints(cbind(pin_streets$xb, pin_streets$yb), proj4string = CRS("+proj=utm +zone=23 +south"))

lalo.a <- spTransform(utm.a, CRS('+init=epsg:4326'))
lalo.b <- spTransform(utm.b, CRS('+init=epsg:4326'))


str_ll <- data.frame(xa = coordinates(lalo.a)[,1],
                     ya = coordinates(lalo.a)[,2],
                     xb = coordinates(lalo.b)[,1],
                     yb = coordinates(lalo.b)[,2])

lon.min <- min(range(c(str_ll$xa, str_ll$xb)))
lon.max <- max(range(c(str_ll$xa, str_ll$xb)))

lat.min <- min(range(c(str_ll$ya, str_ll$yb)))
lat.max <- max(range(c(str_ll$ya, str_ll$yb)))

mun_dom <- extent(lon.min, lon.max, lat.min, lat.max)

# Pinherios AQS coordinates
pin <- data.frame(lat=-23.5614,
                  lon=-46.7020)
coordinates(pin) <- ~lon + lat

lcz_pin <- extract(mun_dom, pin)


# Building building height raster: replacing LCZ by building height value

wu_bh <- crop(wu_geo, mun_dom)
wu_bh_val <- unique(values(wu_bh))

pin_bh_raster <- subs(wu_bh, lcz, by=1, which=2)



# Av Paulista domain ------------------------------------------------------

# Loading Pinheiros neighborhood
pau_streets <-  read.table('02_data/emiss/AVPAUUGS.traf.2014100800',
                           header = T)

# Retrieving link start and end coordinates
pau_streets <- pau_streets[, c('xa', 'xb', 'ya', 'yb')]
pau_utm.a <- SpatialPoints(cbind(pau_streets$xa, pau_streets$ya), proj4string = CRS("+proj=utm +zone=23 +south"))
pau_utm.b <- SpatialPoints(cbind(pau_streets$xb, pau_streets$yb), proj4string = CRS("+proj=utm +zone=23 +south"))

pau_lalo.a <- spTransform(pau_utm.a, CRS('+init=epsg:4326'))
pau_lalo.b <- spTransform(pau_utm.b, CRS('+init=epsg:4326'))

cc_aqs <- data.frame(lat=-23.5535,
                     lon=-46.6727)


pau_str_ll <- data.frame(xa = coordinates(pau_lalo.a)[,1],
                         ya = coordinates(pau_lalo.a)[,2],
                         xb = coordinates(pau_lalo.b)[,1],
                         yb = coordinates(pau_lalo.b)[,2])

pau_lon.min <- min(range(c(pau_str_ll$xa, pau_str_ll$xb, cc_aqs$lon)))
pau_lon.max <- max(range(c(pau_str_ll$xa, pau_str_ll$xb, cc_aqs$lon)))

pau_lat.min <- min(range(c(pau_str_ll$ya, pau_str_ll$yb, cc_aqs$lat)))
pau_lat.max <- max(range(c(pau_str_ll$ya, pau_str_ll$yb, cc_aqs$lat)))

pau_mun_dom <- extent(pau_lon.min, pau_lon.max, pau_lat.min, pau_lat.max)


coordinates(cc_aqs) <- ~lon + lat
lcz_pau <- extract(pau_mun_dom, cc_aqs)


# Building building height raster: replacing LCZ by building height value

pau_wu_bh <- crop(wu_geo, pau_mun_dom)
pau_wu_bh_val <- unique(values(pau_wu_bh))

pau_bh_raster <- subs(pau_wu_bh, lcz, by=1, which=2)


# Map plots ---------------------------------------------------------------


library(RColorBrewer)


bh_cuts <- seq(0, 50, 5)
pal <- colorRampPalette(c('grey', 'black'))

xl <- seq(-46.685, -46.705, length.out = 5)
yl <- seq(-23.575, -23.560, length.out = 4)

pdf("03_output/test_figs/pin_mun_domain.pdf")
raster::plot(pin_bh_raster, col=pal(11), breaks=bh_cuts, axes=F,
             legend.width=1.25,
             legend.args=list(text='(m)', line = 0.5,
                              cex=1.3),
             axis.args = list(cex.axis=1.3),
             asp=1)
segments(str_ll$xa, str_ll$ya, str_ll$xb, str_ll$yb, col = 'red', lwd = 2.5)
points(-46.7020, -23.5614, pch=19, cex = 1.8)
points(-46.7020, -23.5614, pch=19, col ='yellow', cex=1.7)
axis(1, at = xl[2:4], label = paste(xl[2:4] * -1, "°W"), cex.axis=1.3)
axis(2, at = yl[1:4], labels = paste(yl[1:4] * -1,"°S"), cex.axis= 1.3)
dev.off()


pau_xl <- seq(-46.670, -46.645, length.out = 6)
pau_yl <- seq(-23.575, -23.550, length.out = 6)

pdf("./03_output/test_figs/av_pau_mun_domain.pdf")
raster::plot(pau_bh_raster, col=pal(11), breaks=bh_cuts, axes=F,
             legend.width=1.25,
             legend.args=list(text='(m)', line = 0.5,
                              cex=1.3),
             axis.args = list(cex.axis=1.3),
             asp=1)
segments(pau_str_ll$xa, pau_str_ll$ya, pau_str_ll$xb, pau_str_ll$yb, col = 'red', lwd = 2.5)
points(cc_aqs$lon, cc_aqs$lat, pch=19, cex = 1.8)
points(cc_aqs$lon, cc_aqs$lat, pch=19, col ='yellow', cex=1.7)
axis(1, at = pau_xl[2:6], label = paste(pau_xl[2:6] * -1, "°W"), cex.axis=1.3)
axis(2, at = pau_yl[2:6], labels = paste(pau_yl[2:6] * -1,"°S"), cex.axis= 1.3)
dev.off()


