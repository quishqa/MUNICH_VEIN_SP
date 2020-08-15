library(openair)


rmsp <- read.table('./02_data/wrf_met_aqs_station/cetesb_rmsp_d03.dat', header = T,
                   sep = ',', stringsAsFactors = F)


# Reading WRFout and Cetesb data

ReadCetWRF <- function(code, cet_wrf){
  path <- "./02_data/wrf_met_aqs_station/"
  file.name <- paste0(path, code, '_', cet_wrf, '.dat')
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


pin_wrf <- wrf[[28]]
pin_cet <- cet[[28]]


PlotMetParam <- function(df_mod, df_obs, var, ylab,
                         legend_pos = "topleft"){
  ylim <- range(c(0, df_mod[[var]], df_obs[[var]]))
  plot(df_obs$date, df_obs[[var]], t = "l", lwd = 3.5, col = "Black",
       ylab = ylab, main = unique(df_mod$name),
       xlab = "2014", ylim =ylim)
  points(df_obs$date, df_obs[[var]], pch = 19, cex = 1.25, col ="Black")
  lines(df_mod$date, df_mod[[var]], lwd = 3.5, col = "red")
  points(df_mod$date, df_mod[[var]], pch = 18, col = "red", cex = 1.25)
  legend(legend_pos, pch = c(19, 18), col = c("black","red"), 
         lty = c(1, 1), lwd = c(3.5, 3.5), bty = "n",
         legend = c("CETESB", "WRF"),
         cex = c(1.25, 1.25))

}


PlotMetParam(pin_wrf, pin_cet, "tc", "Temperature (ºC)")
PlotMetParam(pin_wrf, pin_cet, "rh", "Relative Humidity (%)", "bottomleft")
PlotMetParam(pin_wrf, pin_cet, "ws", "Wind Speed (m/s)", "bottomleft")



all_cet <- do.call(rbind, cet)
all_wrf <- do.call(rbind, wrf)
names(all_wrf)[2:5] <-  paste0('mod.', names(all_wrf)[2:5])


all_data <- cbind(all_cet, all_wrf[2:6])


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
  return(data.frame(default = "all data",
                    n = N, 
                    FAC2 = NA,
                    MB = MB, 
                    MGE=MAGE,
                    NMB = NA,
                    NMGE = NA,
                    RMSE = NA,
                    r = NA,
                    COE =NA,
                    IOA = NA))
}

wind.dir <- WindDirectionPerfo(all_data)
wind.dir


wind.speed <- subset(all_data, ws >= 0.5)

tc_perf <- modStats(all_data, obs = 'tc', mod = 'mod.tc')
rh_perf <- modStats(all_data, obs = 'rh', mod = 'mod.rh')

ws_perf <- modStats(wind.speed, obs = 'ws', mod = 'mod.ws')
wd_perf <- wind.dir <- WindDirectionPerfo(all_data)

all_perf <- do.call(rbind, list(tc_perf, rh_perf, ws_perf, wd_perf))
write.table(all_perf, "./03_output/text_files/wrf_met_perf.csv", sep = ",",
            row.names = F)

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


all_data_tc <- all_data[, c("tc", "mod.tc")]
all_data_rh <- all_data[, c("rh", "mod.rh")]
all_data_ws <- wind.speed[, c("ws", "mod.ws")]

all_data_tc_cc <- all_data_tc[complete.cases(all_data_tc), ]
all_data_rh_cc <- all_data_rh[complete.cases(all_data_rh), ]
all_data_ws_cc <- all_data_ws[complete.cases(all_data_ws), ]


pielke <- data.frame(
  tc = c(mean(all_data_tc_cc$tc, na.rm = T),
         mean(all_data_tc_cc$mod.tc, na.rm =T),
         sd(all_data_tc_cc$tc, na.rm = T),
         sd(all_data_tc_cc$mod.tc, na.rm = T),
         as.numeric(modStats(all_data_tc_cc, mod = "mod.tc", obs="tc", statistic = "RMSE")[1, 2]),
         RMSE_ub(all_dataa_tc_cc, 'mod.tc', 'tc')),
  rh = c(mean(all_data_rh_cc$rh, na.rm = T),
         mean(all_data_rh_cc$mod.rh, na.rm =T),
         sd(all_data_rh_cc$rh, na.rm = T),
         sd(all_data_rh_cc$mod.rh, na.rm = T),
         as.numeric(modStats(all_data_rh_cc, mod = "mod.rh", obs="rh", statistic = "RMSE")[1, 2]),
         RMSE_ub(all_data_rh_cc, 'mod.rh', 'rh')),
  ws = c(mean(all_data_ws_cc$ws, na.rm = T),
         mean(all_data_ws_cc$mod.ws, na.rm =T),
         sd(all_data_ws_cc$ws, na.rm = T),
         sd(all_data_ws_cc$mod.ws, na.rm = T),
         as.numeric(modStats(all_data_ws_cc, mod = "mod.ws", obs="ws", statistic = "RMSE")[1, 2]),
         RMSE_ub(all_data_ws_cc, 'mod.ws', 'ws'))
  
)
pielke

write.table(pielke, "./03_output/text_files/pielke_statistcs.csv", sep = ",",
            row.names = F)
save.image('02_data/met_validation.Rda')
