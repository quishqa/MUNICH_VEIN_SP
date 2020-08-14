library(openair)


rmsp <- read.table('./cetesb_rmsp_d03.dat', header = T,
                   sep = ',', stringsAsFactors = F)


# Reading WRFout and Cetesb data

ReadCetWRF <- function(code, cet_wrf){
  file.name <- paste0(code, '_', cet_wrf, '.dat')
  df <- read.table(file.name, header = T, sep = ',', 
                   stringsAsFactors = F, dec = '.')
  if (cet_wrf == 'cetesb'){
    df$date <- as.POSIXct(strptime(df$date, '%Y-%m-%d %H:%M'), tz = 'America/Sao_Paulo')
    attributes(df$date)$tzone <- 'UTC'
    df$wd[df$wd  == 888 | df$wd  == 777] <- NA
  } else if (cet_wrf == 'wrfout') {
    df$date <- as.POSIXct(strptime(df$date, '%Y-%m-%d %H:%M'), tz = 'UTC')
  }
  return(df)
}

cet <- lapply(rmsp$code, ReadCetWRF, cet_wrf = 'cetesb')
wrf <- lapply(rmsp$code, ReadCetWRF, cet_wrf = 'wrfout')

# Clipping by simulation time

sim.date <- seq(as.POSIXct('2014-10-06 00:00 00:00', tz = 'UTC'),
                by = 'hour', 
                length.out = 168)

cet <- lapply(cet, function(df) df[df$date %in% sim.date, ])
wrf <- lapply(wrf, function(df) df[df$date %in% sim.date, ])


all_cet <- do.call(rbind, cet)
all_wrf <- do.call(rbind, wrf)
names(all_wrf)[2:5] <-  paste0('mod.', names(all_wrf)[2:5])


all_data <- cbind(all_cet, all_wrf[2:6])

# temperature
modStats(all_data, obs = 'tc', mod = 'mod.tc')
# modStats(all_data, obs = 'tc', mod = 'mod.tc', type='name')

# Relative humidity
modStats(all_data, obs = 'rh', mod = 'mod.rh')
modStats(all_data, obs = 'rh', mod = 'mod.rh', type='name')

# Wind Speed
modStats(all_data, obs = 'ws', mod = 'mod.ws')
modStats(all_data, obs = 'ws', mod = 'mod.ws', type='name')

# New wind direction MB and MAGE

WindDirectionD <- function(mod, obs){
  if (!is.na(obs)) {
    if (mod < obs){
      if (abs(mod - obs) < abs(360 + (mod -obs))){
        D = mod - obs
      } else if (abs(mod - obs) > abs(360 + (mod -obs))){
        D = 360 + (mod - obs)
      }
    } else if (mod >= obs) {
      if (abs(mod - obs) < abs((mod - obs) - 360)){
        D = mod - obs
      } else if (abs(mod - obs) > abs((mod - obs) - 360)){
        D = (mod - obs) - 360
      }
    }
  } else {
    D = NA
  }
  return(D)
}


WindDirectionPerfo <- function(df){
  df <- subset(df, ws >= 0.5)
  df <- df[c('mod.wd', 'wd')]
  D <- apply(as.matrix(df), 1, function(df) WindDirectionD(df[1], df[2]))
  N <- sum(!is.na(D))
  MB <- mean(D, na.rm = T)
  MAGE <- mean(abs(D), na.rm = T)
  return(data.frame(N = N, MB = MB, MAGE=MAGE))
}

wind.dir <- WindDirectionPerfo(all_data)
wind.dir


# df <- subset(all_data, name == 'Pinheiros')
# df2 <- df[c('mod.wd', 'wd')]
# 
# a <- apply(as.matrix(df2), 1, function(df) WindDirectionD(df[1], df[2]))

wind.speed <- subset(all_data, ws >= 0.5)

modStats(all_data, obs = 'tc', mod = 'mod.tc')
modStats(all_data, obs = 'rh', mod = 'mod.rh')
modStats(all_data, obs = 'ws', mod = 'mod.ws')
modStats(wind.speed, obs = 'ws', mod = 'mod.ws')
wind.dir <- WindDirectionPerfo(all_data)

TaylorDiagram(na.omit(all_data), obs = 'tc', mod='mod.tc', group = 'name')
TaylorDiagram(na.omit(all_data), obs = 'rh', mod='mod.rh', group = 'name')
TaylorDiagram(na.omit(all_data), obs = 'ws', mod='mod.ws', group = 'name')

# Pielke criteria

sd(all_data$tc, na.rm = T)
sd(all_data$mod.tc, na.rm = T)

sd(all_data$rh, na.rm = T)
sd(all_data$mod.rh, na.rm = T)

sd(all_data$ws, na.rm = T)
sd(all_data$mod.ws, na.rm = T)

RMSE_ub <- function(df, mod, obs){
  mod_av <- mean(df[[mod]], na.rm = T)
  obs_av <- mean(df[[obs]], na.rm = T)
  
  mod_1 <- all_data[[mod]] - mod_av
  obs_1 <- all_data[[obs]] - obs_av
  
  diff <- (mod_1 - obs_1)^2
  rmse_ub <- sqrt(mean(diff, na.rm = T))
  return(rmse_ub)
}



RMSE_ub(all_data, 'mod.ws', 'ws')
RMSE_ub(all_data, 'mod.tc', 'tc')
RMSE_ub(all_data, 'mod.rh', 'rh')

save.image('met_validation.Rda')
