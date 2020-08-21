# Pinheiros emission profile
# for all street links.

library(sf)
library(ggplot2)

GetEmissProf <- function(files){
  emiss <- lapply(files,
                  function(x) read.table(x, header = T, stringsAsFactors = F, dec = '.'))
  emiss <- lapply(emiss, function(df) return(df[, c('xa', 'ya', 'xb', 'yb','NOx', 'NMHC')]))
  emiss.m <- lapply(emiss, colMeans)
  emiss.w <- do.call(rbind, emiss.m)
  
  
  emiss.w <- data.frame(nox = emiss.w[, 5], voc = emiss.w[, 6])
  emiss.w$date <- seq(as.POSIXct('2014-10-08 00:00', tz = 'UTC'),
                      as.POSIXct('2014-10-08 23:00', tz = 'UTC'),
                      by = 'hour')
  attr(emiss.w$date, 'tzone') <- 'America/Sao_Paulo'
  emiss.w$h <- format(emiss.w$date, format = '%H')
  pp <- emiss.w
  # p1 <- emiss.w[10:24, ]
  # p2 <- emiss.w[1:9, ]
  # pp <- rbind(p1, p2)
  return(pp)
}


GetEmissSF <- function(files, tt){
  emiss <- lapply(files,
                  function(x) read.table(x, header = T, stringsAsFactors = F, dec = '.'))
  emiss <- lapply(emiss, function(df) return(df[, c('i','xa', 'ya', 'xb', 'yb','NOx', 'NMHC')]))
  coords <- emiss[[1]][, c('xa', 'ya', 'xb', 'yb')]
  alist <- apply(coords, 1, list)
  amat <- lapply(alist, function(m) matrix(unlist(m), 2, 2, byrow = T))
  asfc <- lapply(amat, st_linestring)
  sfc <- st_sfc(asfc, crs = 31983)
  sfc <- st_transform(sfc, crs = 4326)
  
  
  sf.t <- st_sf(emiss[[tt]], geometry = sfc)
  return(sf.t)
}


files.day <- list.files('./02_data/emiss/', pattern = 'PINUGS.traf.20141008', full.names = T)
files.end <- list.files('./02_data/emiss/', pattern = 'PINUGS.traf.20141011', full.names = T)

p.day <- GetEmissProf(files.day)
p.end <- GetEmissProf(files.end)

PlotEmissProf <- function(p.day, p.end, pol, ylab.name, leg = 'topright'){
  plot(p.day[[pol]], t = 'l', lwd = 2.5, col = 'red', axes = F,
       ylim = range(p.day[[pol]], p.end[[pol]]),
       ylab = '',
       xlab = '')
  points(p.day[[pol]], col = "red", pch = 19)
  lines(p.end[[pol]], t = 'l', lwd = 2.5, col = 'blue')
  points(p.end[[pol]], pch = 18,  col = 'blue', cex = 1.5)
  axis(2)
  axis(1, at =1:24, labels=0:23)
  title(ylab = ylab.name, line = 2.3,
        xlab = 'Hours (Local time)', cex.lab = 1.25)
  legend(leg, bty = 'n', legend = c('Weekday', 'Weekend'),
         lwd = 2.5, lty = 1, col = c('red', 'blue'), pch = c(19, 18), pt.cex = 1.25)
  box()
}


svg('03_output/paper_figs/Fig_01_emissions_profile.svg', height = 6., width = 14)
par(mfrow = c(1, 2))
PlotEmissProf(p.day, p.end, 'nox', expression("NO"[X] * " (" * mu * "g km" ^-1 *  "h" ^-1*")"), "topleft")
mtext("(a)", adj = 0, line=0.20, cex = 1.25)
PlotEmissProf(p.day, p.end, 'voc', expression("VOCs" * " (" * mu * "g km" ^-1*  "h"^ -1 *")"), 'bottomright')
mtext("(b)", adj = 0, line=0.20, cex = 1.25)
dev.off()