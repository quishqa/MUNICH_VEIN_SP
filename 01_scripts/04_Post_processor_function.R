library(openair)
library(rgdal)
library(sf)
library(ggplot2)

LoadMunichOutput <- function(all.path, pattern){
  files <- list.files(all.path, pattern = pattern, full.names = T )
  files.dates <- list.files(all.path, pattern = pattern)
  
  output <- lapply(files, 
                   function(x) read.table(x,header = F, sep = '\t', 
                                          stringsAsFactors = F,
                                          dec = '.', 
                                          col.names = c('link', 'o3', 'no', 'no2', 'nothing')))
  
  h.means <- lapply(output, colMeans)
  h.df <- as.data.frame(do.call(rbind, h.means))
  h.df$date <- as.POSIXct(strptime(files.dates, "%Y%m%d_%H-%M"), tz = 'UTC')
  h.df <- h.df[, c('date', 'o3', 'no', 'no2')]
  h.df$nox <- h.df$no * 46 / 30 + h.df$no2
  return(h.df)
}

LoadMunichOutput2LT <- function(all.path, pattern){
  files <- list.files(all.path, pattern = pattern, full.names = T )
  files.dates <- list.files(all.path, pattern = pattern)
  
  output <- lapply(files, 
                   function(x) read.table(x,header = F, sep = '\t', 
                                          stringsAsFactors = F,
                                          dec = '.', 
                                          col.names = c('link', 'o3', 'no', 'no2', 'nothing')))
  
  h.means <- lapply(output, colMeans)
  h.df <- as.data.frame(do.call(rbind, h.means))
  h.df$date <- as.POSIXct(strptime(files.dates, "%Y%m%d_%H-%M"), tz = 'UTC')
  attributes(h.df$date)$tzone <- 'America/Sao_Paulo'
  h.df <- h.df[, c('date', 'o3', 'no', 'no2')]
  h.df$nox <- h.df$no * 46 / 30 + h.df$no2
  return(h.df)
}


LoadMunichOutputStreet <- function(all.path, pattern, streets){
  files <- list.files(all.path, pattern = pattern, full.names = T )
  files.dates <- list.files(all.path, pattern = pattern)
  
  output <- lapply(files,
                   function(x) read.table(x,header = F, sep = '\t',
                                          stringsAsFactors = F,
                                          dec = '.',
                                          col.names = c('link', 'o3', 'no', 'no2', 'nothing')))
  output <- lapply(output, function(df) df[df$link %in% streets, ])
  
  h.means <- lapply(output, colMeans)
  h.df <- as.data.frame(do.call(rbind, h.means))
  
  h.df$date <- as.POSIXct(strptime(files.dates, "%Y%m%d_%H-%M"), tz = 'UTC')
  h.df <- h.df[, c('date', 'o3', 'no', 'no2')]
  h.df$nox <- h.df$no * 46 / 30 + h.df$no2
  return(h.df)
}

LoadMunichOutputStreet2LT <- function(all.path, pattern, streets){
  files <- list.files(all.path, pattern = pattern, full.names = T )
  files.dates <- list.files(all.path, pattern = pattern)
  
  output <- lapply(files,
                   function(x) read.table(x,header = F, sep = '\t',
                                          stringsAsFactors = F,
                                          dec = '.',
                                          col.names = c('link', 'o3', 'no', 'no2', 'nothing')))
  output <- lapply(output, function(df) df[df$link %in% streets, ])
  
  h.means <- lapply(output, colMeans)
  h.df <- as.data.frame(do.call(rbind, h.means))
  
  h.df$date <- as.POSIXct(strptime(files.dates, "%Y%m%d_%H-%M"), tz = 'UTC')
  attributes(h.df$date)$tzone <- 'America/Sao_Paulo'
  h.df <- h.df[, c('date', 'o3', 'no', 'no2')]
  h.df$nox <- h.df$no * 46 / 30 + h.df$no2
  return(h.df)
}


LoadBackground <- function(file.path){
  bg.df <- read.table(file.path, skip = 1, sep = '\t', stringsAsFactors = F, 
                      col.names = c('date','o3', 'no2', 'no'))
  bg.df$nox <- bg.df$no * 46 / 30 + bg.df$no2
  bg.df$date <- as.POSIXct(strptime(bg.df$date, '%Y-%m-%d_%H'),
                           tz = 'UTC')
  return(bg.df)
}

LoadBackgroundUTC2LT <- function(file.path){
  bg.df <- read.table(file.path, skip = 1, sep = '\t', stringsAsFactors = F, 
                      col.names = c('date','o3', 'no2', 'no'))
  bg.df$nox <- bg.df$no * 46 / 30  + bg.df$no2
  bg.df$date <- as.POSIXct(strptime(bg.df$date, '%Y-%m-%d_%H'),
                           tz = 'UTC')
  attributes(bg.df$date)$tzone <- 'America/Sao_Paulo'
  return(bg.df)
}


LoadObservationsUTC <- function(file.path){
  df <- read.table(file.path, header=T, sep=',',
                   stringsAsFactors = F, 
                   col.names = c('date', 'o3', 'no', 'no2'))
  df$nox <- df$no * 46 / 30 + df$no2
  df$date <- as.POSIXct(strptime(df$date, '%Y-%m-%d %H:%M:%S'), tz = 'UTC')
  return(df)
}

LoadObservationsLocal2UTC <- function(file.path){
  df <- read.table(file.path, header=T, sep=',',
                   stringsAsFactors = F, 
                   col.names = c('date', 'o3', 'no', 'no2'))
  df$nox <- df$no * 46 / 30 + df$no2
  df$date <- as.POSIXct(strptime(df$date, '%Y-%m-%d %H:%M:%S'), tz = 'America/Sao_Paulo')
  attributes(df$date)$tzone <- 'UTC'
  return(df)
}

LoadObservationsLT <- function(file.path){
  df <- read.table(file.path, header=T, sep=',',
                   stringsAsFactors = F, 
                   col.names = c('date', 'o3', 'no', 'no2'))
  df$nox <- df$no * 46 / 30 + df$no2
  df$date <- as.POSIXct(strptime(df$date, '%Y-%m-%d %H:%M:%S'), tz = 'America/Sao_Paulo')
  return(df)
}



PerfomStats <- function(mun.res, mun.obs, pol, ff){
  df.per <- data.frame(date = mun.res$date,
                       obs = mun.obs[mun.obs$date %in% mun.res$date, ][[pol]],
                       mod =mun.res[[pol]])
  ans <- modStats(df.per)
  ans$SdM <- sd(df.per$mod, na.rm = T)
  ans$SdO <- sd(df.per$obs, na.rm = T)
  ans$MM  <- mean(df.per$mod, na.rm = T)
  ans$MO  <- mean(df.per$obs, na.rm = T)
  file.name <- (paste0(ff, '_', pol, '_perf.dat'))
  write.table(format(ans, digits = 3), file.name, quote = F,
              row.names = F, sep = ',')
  return(ans)
}


PerfomStatsNS <- function(mun.res, mun.obs, pol){
  df.per <- data.frame(date = mun.res$date,
                       obs = mun.obs[mun.obs$date %in% mun.res$date, ][[pol]],
                       mod =mun.res[[pol]])
  ans <- modStats(df.per)
  
  df.per <- df.per[complete.cases(df.per), ]
  ans$SdM <- sd(df.per$mod, na.rm = T)
  ans$SdO <- sd(df.per$obs, na.rm = T)
  ans$MM  <- mean(df.per$mod, na.rm = T)
  ans$MO  <- mean(df.per$obs, na.rm = T)
  # file.name <- (paste0(ff, '_', pol, '_perf.dat'))
  # write.table(format(ans, digits = 3), file.name, quote = F,
  #             row.names = F, sep = ',')
  return(ans)
}


HannaAndChangComCases <- function(mun.obs, mun.sim, pol){
  df <- data.frame(obs = mun.obs[[pol]],
                   mod = mun.sim[[pol]])
  df <- df[complete.cases(df), ]
  
  fb <- 2 * mean(df$obs - df$mod, na.rm = T) / (mean(df$obs, na.rm = T) + mean(df$mod, na.rm = T))
  nmse <- mean((df$obs - df$mod)^2, na.rm = T) / (mean(df$obs, na.rm = T) * mean(df$mod, na.rm = T))
  mg <- exp(mean(log(df$obs), na.rm = T) - mean(log(df$mod), na.rm = T))
  vg <- exp(mean((log(df$obs) - log(df$mod))^2, na.rm = T))
  nad <- abs(mean(df$obs - df$mod, na.rm = T))/(mean(df$obs, na.rm = T) + mean(df$mod, na.rm = T))
  
  ans <- data.frame(fb = fb,
                    nmse = nmse,
                    nad = nad,
                    mg = mg,
                    vg = vg,
                    m_obs = mean(df$obs),
                    m_mod = mean(df$mod),
                    sd_o = sd(df$obs),
                    sd_m = sd(df$mod))
  return(ans)
}

AllHannaAndChangComCase <- function(mun.obs, mun.sim){
  o3 <- HannaAndChangComCases(mun.obs, mun.sim, 'o3')
  nox <- HannaAndChangComCases(mun.obs, mun.sim, 'nox')
  no <- HannaAndChangComCases(mun.obs, mun.sim, 'no')
  no2 <- HannaAndChangComCases(mun.obs, mun.sim, 'no2')
  
  ans <- do.call(rbind, list(o3, nox, no, no2))
  ans$pol <- c('o3', 'nox', 'no', 'no2')
  
  return(ans)
}

AllPerformaceTable <- function(mun_obs, mun_res, ff){
  o3_per <- PerfomStatsNS(mun_res, mun_obs, 'o3')
  nox_per <- PerfomStatsNS(mun_res, mun_obs, 'nox')
  no2_per <- PerfomStatsNS(mun_res, mun_obs, 'no')
  no_per <- PerfomStatsNS(mun_res, mun_obs, 'no2')
  
  ms_per <- do.call(rbind, list(o3_per, nox_per, no2_per, no_per))
  
  hc_per <- AllHannaAndChangComCase(mun_obs, mun_res)
  hc_per$fb_abs <- abs(hc_per$fb)
  hc_per$fac2 <- ms_per$FAC2
  
  complete_stats <- cbind(ms_per, hc_per)
  
  a <- ms_per[, c('MM', 'MO', 'SdM', 'SdO', 'MB', 'NMB', 'NMGE', 'RMSE', 'r')]
  b <- hc_per[, c('fb_abs', 'nmse', 'fac2', 'nad', 'pol')]
  
  all_per <- cbind(a,b)
  all_per <- all_per[, c(14, 1:13)]
  file_name <- paste0(ff, '_all_perf.csv')
  write.table(all_per, file = file_name, sep = ',', row.names = F)
  invisible(complete_stats)
}

Links2Point <- function(coords.zone, coords.point, n.links ){
  zn <- read.table(coords.zone, header = T)
  zn.st <- zn[, c('i', 'xa', 'xb', 'ya', 'yb')]
  
  utm.a <- SpatialPoints(cbind(zn.st$xa, zn.st$ya), proj4string = CRS("+proj=utm +zone=23 +south"))
  utm.b <- SpatialPoints(cbind(zn.st$xb, zn.st$yb), proj4string = CRS("+proj=utm +zone=23 +south"))
  
  lalo.a <- spTransform(utm.a, CRS('+proj=longlat'))
  lalo.b <- spTransform(utm.b, CRS('+proj=longlat'))
  
  df <- data.frame(id = zn$i,
                   xa = lalo.a@coords[,1],
                   ya = lalo.a@coords[,2],
                   xb = lalo.b@coords[,1],
                   yb = lalo.b@coords[,2])
  pt <- coords.point
  
  df$dist <- sqrt((pt[1] - df$xa)^2 + (pt[2] - df$ya)^2)
  
  near.link <- head(df[order(df$dist),], n.links)
  
  return(near.link$id)
}




PlotResultsAllLinks <- function(mun.res, mun.bg, mun.obs,
                                pol, ff) {
  pdf(paste0(ff, '_', pol, '.pdf'))
  cols <- c('red', 'blue', 'black')
  plot(mun.res$date, mun.res$o3, t = 'l', lwd = 1.5, col=cols[1], 
       xlab='October 2014', ylab='ug/m3',
       main = paste(toupper(pol), ff, sep = ' - '),
       ylim = range(c(mun.res[[pol]], mun.bg[[pol]], mun.obs[[pol]]), na.rm = T))
  lines(mun.res$date, mun.bg[mun.bg$date %in% mun.res$date, ][[pol]],
        t='l', col=cols[2], lwd = 1.5)
  lines(mun.res$date, mun.obs[mun.obs$date %in% mun.res$date, ][[pol]], 
        t='l', col=cols[3], lwd = 1.5)
  legend('topleft', legend = c('MUNICH', 'Background', 'Obs.'),
         lty = c(1, 1, 1), col = cols, lwd = c(1.5, 1.5, 1.5),
         bty = 'n')
  dev.off()
}



MunOutput2Sf <- function(mun.coords, mun.out, crs = 31983){
  
  coords <- mun.coords[, c('xa', 'ya', 'xb', 'yb')]
  
  alist <- apply(coords, 1, list)
  amat <- lapply(alist, function(m) matrix(unlist(m), 2, 2, byrow = T))
  asfc <- lapply(amat, st_linestring)
  
  sfc <- st_sfc(asfc, crs = crs)
  
  res <- data.frame(o3 = mun.out$o3,
                    no = mun.out$no,
                    no2 = mun.out$no2)
  res$nox <- res$no * 46 / 30 + res$no2
  
  sf <- st_sf(res, sfc)
  sf <- st_transform(sf, crs = 4326)
  return(sf)
}



StreetPlotMun <- function(sf, pol, est, subtitle){
  ggplot()+
    geom_sf(data=sf[pol], aes(col=pol), lwd = 1)+
    geom_sf(data=est, size = 2.5, shape = 23, fill = 'Red' )+
    # scale_colour_gradientn(colors = rev(inferno(20))) +
    scale_colour_viridis_c(name=expression("[" * mu * "g m" ^-3 * "]"),
                           option = 'B', direction = -1,
                           limits = c(0, 30),
                           breaks = seq(0, 30, 5)) +
    theme_bw() +
    ggtitle(toupper(pol), subtitle = 'Pinheiros')
}



pol.range <- function(obs, bg, mun.out, pol){
  dom <- range(c(obs[[pol]],
                 bg[[pol]],
                 mun.out[[pol]]), na.rm = T)
  return(dom)
}



PlotTemporlPolBig <- function(mun.obs, mun.bg, mun.out, pol, ylab.name){
  dates <- mun.out$date
  index <- which(format(dates, format = "%H") == "00")
  xlab_at <- as.numeric(dates[index])
  xlab_names <- format(dates, format = "%d")
  xlab_labels <- xlab_names[index]
  plot(mun.obs$date, mun.obs[[pol]], t = 'l', lwd = 2.5, col = 'black',
       ylab = '', 
       xlab = '', 
       ylim = pol.range(mun.obs, mun.bg, mun.out, pol),
       main = '',
       axes = F)
  lines(mun.bg$date, mun.bg[[pol]], t = 'l', lwd = 2.5, lty = 1, col = 'blue')
  lines(mun.out$date, mun.out[[pol]], t = 'l', lwd =2.5, col = 'red')
  title(ylab = ylab.name, line = 2.3,
        xlab = 'Oct. 2014', cex.lab = 1.8)
  axis(2, cex.axis = 1.8)
  axis(1, at = xlab_at,  labels = xlab_labels, cex.axis = 1.8)
  box()
  legend('topleft', legend = c('Observation', 'Background', 'MUNICH'),
         col = c('black', 'blue', 'red'), lwd = c(2.5, 2.5, 2.5),
         lty = c(1, 1, 1), bty = 'n', cex = 1.5)
}

HourlyProf <- function(df){
  attr(df$date, 'tzone') <- 'America/Sao_Paulo'
  h.df <- aggregate(df[c('o3', 'no', 'no2', 'nox')], format(df['date'], '%H'),
                    mean, na.rm = T)
  return(h.df)
}

SortHour <- function(df.h){
  p6_23 <- df.h[7:24,]
  p0_5 <- df.h[0:6,]
  df6_6 <- rbind(p6_23, p0_5)
  return(df6_6)
}

PlotDayProfCompSplit <- function(obs, bg, mun, pol, ylim, ylab.name,  leg.pos = 'topright'){
  obs <- SortHour(obs)
  bg <- SortHour(bg)
  mun <- SortHour(mun)
  plot(obs[[pol]], t = 'l', lwd = 2.5, col = 'Black',
       ylim = ylim,
       ylab = '',
       xlab = '',
       axes = F)
  lines(mun[[pol]], col = 'Red', lwd = 2.5)
  lines(bg[[pol]], col = 'Blue', lwd = 2.5)
  title(ylab = ylab.name, line = 2.3,
        xlab = 'Hours (Local time)', cex.lab = 1.8)
  legend(leg.pos, lty = 1, lwd = 2.5, col = c('black', 'blue', 'red'),
         legend = c('Observation', 'Background', 'MUNICH'),
         bty = 'n', cex = 1.8)
  axis(2, cex.axis = 1.8)
  axis(1, at = 1:24, labels = c(6:23, 0:5), cex.axis = 1.8)
  box()
}

PlotDayProfComp <- function(obs, bg, mun, pol, ylim, ylab.name,  leg.pos = 'topright'){
  plot(obs[[pol]], t = 'l', lwd = 2.5, col = 'Black',
       ylim = ylim,
       ylab = '',
       xlab = '',
       axes = F)
  points(obs[[pol]], pch = 19, col = "Black", cex = 1.125)
  
  lines(mun[[pol]], col = 'Red', lwd = 2.5)
  points(mun[[pol]], pch = 17, col = 'Red', cex = 1.25)
  
  lines(bg[[pol]], col = 'Blue', lwd = 2.5, cex = 1.25)
  points(bg[[pol]], pch =15, col = 'Blue')
  
  title(ylab = ylab.name, line = 2.3,
        xlab = 'Hours (Local time)', cex.lab = 1.8)
  legend(leg.pos, lty = 1, lwd = 2.5, col = c('black', 'blue', 'red'),
         legend = c('Observation', 'Background', 'MUNICH'),
         pch = c(19, 15, 17),
         bty = 'n', cex = 1.8)
  axis(2, cex.axis = 1.8)
  axis(1, at = 1:24, labels = 0:23, cex.axis = 1.8)
  box()
}

# PlotTemporlPol <- function(mun.obs, mun.bg, mun.out, pol, zone){
#   plot(mun.obs$date, mun.obs[[pol]], t = 'l', lwd = 1.5, col = 'black',
#        ylab = '', 
#        xlab = '', 
#        ylim = pol.range(mun.obs, mun.bg, mun.out, pol),
#        main = paste0(zone, ' - ', toupper(pol)),
#        axes = F)
#   lines(mun.bg$date, mun.bg[[pol]], t = 'l', lwd = 1.5, lty = 1, col = 'blue')
#   lines(mun.out$date, mun.out[[pol]], t = 'l', lwd =1.5, col = 'red')
#   title(ylab = expression('[' *mu*'g m' ^-3*']'), line = 2.3,
#         xlab = 'Oct. 2014', cex.lab = 1.25)
#   axis(2, cex.axis = 1.25)
#   axis(1, at = as.numeric(mun.obs$date[seq(1, 168, 24)]),
#        labels = format(mun.obs$date[seq(1, 168, 24)], 
#                        '%d'), cex.axis = 1.25)
#   box()
#   legend('topleft', legend = c('Observation', 'Background', 'MUNICH'),
#          col = c('black', 'blue', 'red'), lwd = c(2, 2, 2),
#          lty = c(1, 1, 1), bty = 'n')
# }









LoadMunResAll <- function(all.path, pattern){
  files <- list.files(all.path, pattern = pattern, full.names = T )
  files.dates <- list.files(all.path, pattern = pattern)
  
  output <- lapply(files, 
                   function(x) read.table(x,header = F, sep = '\t', 
                                          stringsAsFactors = F,
                                          dec = '.', 
                                          col.names = c('link', 'o3', 'no', 'no2', 'nothing')))
  output <- lapply(output, function(df) df[, c('link', 'o3', 'no', 'no2')])
  
  return(output)
}



DayAndNightMUN <- function(all.path, pattern, OUT, period){
  files.dates <- list.files(all.path, pattern = pattern)
  all_dates <- data.frame(i = seq(1, length(files.dates)),
                          date = files.dates)
  all_dates$date <- as.POSIXct(strptime(all_dates$date, format = '%Y%m%d_%H-%M', tz = 'UTC'))
  attr(all_dates$date, 'tzone') <- 'Americas/Sao_Paulo'
  all_dates$h <- format(all_dates$date, format='%H')
  day <- sprintf('%02d', seq(0, 23))
  dayl <- day[7:19]
  night <- day[!(day %in% dayl)]
  
  light <- all_dates[all_dates$h %in% dayl, ]
  night <- all_dates[all_dates$h %in% night, ]
  
  if (period == 'D'){
    res <- OUT[light$i]
  } else if (period == 'N'){
    res <- OUT[night$i]
  }
  return(res)
}


  