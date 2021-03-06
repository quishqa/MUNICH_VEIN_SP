# Comparison between observation and MUNICH simulation scenario
# with only NOX emission incremented by four,  for 
# Pinheiros neighborhood

source("./01_scripts/06_Post_processor_functions.R")

# Considering Pin urban canyon
pin_est <- c(-46.7020, -23.5614)
pin_est_sf <- st_sfc(st_point(pin_est), crs = 4326)
pin_coords_file <- "./02_data/emiss/PINUGS.traf.2014100804"
coord <- read.table(pin_coords_file, header = T,
                    stringsAsFactors = F)
coord <- coord[c("xa", "ya", "xb", "yb")]


ibi_bg <- LoadBackgroundUTC2LT("03_output/text_files/ibi_bg_o3_no2_no.dat")

pin_obs <- LoadObservationsLT('02_data/bg_obs/pin_oct_14.csv')

pin_mun_uc <- LoadMunichOutputStreet2LT("02_data/tests/pin_ugs_nox_x4/", "201410",
                                        Links2Point(pin_coords_file, pin_est, 25))

mun_obs <- pin_obs[pin_obs$date %in% pin_mun_uc$date, ]
mun_bg_ibi  <- ibi_bg[ibi_bg$date %in% pin_mun_uc$date, ]

# Temporal Series
svg("03_output/paper_figs/Fig_s3_Pin_ugs_nox_x4.svg", height = 10, width = 14)
par(mfrow=c(2, 2), mar=c(3.2,4.6, 1.5, 0.7))
PlotTemporlPolBig(mun_obs, mun_bg_ibi, pin_mun_uc, 'o3', expression("O"[3] * " (" * mu * "g m" ^-3 * ")"))
mtext("(a)", adj = 0, line=0.20, cex = 1.25)
PlotTemporlPolBig(mun_obs, mun_bg_ibi, pin_mun_uc, 'nox', expression("NO"[X] * " (" * mu * "g m" ^-3 * ")"))
mtext("(b)", adj = 0, line=0.20, cex = 1.25)
PlotTemporlPolBig(mun_obs, mun_bg_ibi, pin_mun_uc, 'no', expression("NO" * " (" * mu * "g m" ^-3 * ")"))
mtext("(c)", adj = 0, line=0.20, cex = 1.25)
PlotTemporlPolBig(mun_obs, mun_bg_ibi, pin_mun_uc, 'no2', expression("NO"[2] * " (" * mu * "g m" ^-3 * ")"))
mtext("(d)", adj = 0, line=0.20, cex = 1.25)
dev.off()

# Daily Profile

obs.h <- HourlyProf(mun_obs)
bg.h <- HourlyProf(mun_bg_ibi)
pin.h <- HourlyProf(pin_mun_uc)

o3.range <- range(c(obs.h$o3, bg.h$o3, pin.h$o3), na.rm = T)
no.range <- range(c(obs.h$no, bg.h$no, pin.h$no), na.rm = T)
no2.range <- range(c(obs.h$no2, bg.h$no2, pin.h$no2), na.rm = T)
nox.range <- range(c(obs.h$nox, bg.h$nox, pin.h$nox), na.rm = T)


svg("03_output/paper_figs/Fig_s4_Pin_ugs_nox_x4.svg", width = 14, height = 10)
par(mfrow = c(2, 2), mar=c(3.2,4.6, 1.5, 0.7))
PlotDayProfComp(obs.h, bg.h, pin.h, 'o3', o3.range, 
                     expression("O"[3] * " (" * mu * "g m" ^-3 * ")"), "topleft")
mtext("(a)", adj = 0, line=0.20, cex = 1.25)
PlotDayProfComp(obs.h, bg.h, pin.h, 'nox', nox.range,
                     expression("NO"[X] * " (" * mu * "g m" ^-3 * ")"), 'topright')
mtext("(b)", adj = 0, line=0.20, cex = 1.25)
PlotDayProfComp(obs.h, bg.h, pin.h, 'no', no.range,
                     expression("NO" * " (" * mu * "g m" ^-3 * ")"), 'topright')
mtext("(c)", adj = 0, line=0.20, cex = 1.25)
PlotDayProfComp(obs.h, bg.h, pin.h, 'no2', c(0, 100),
                     expression("NO"[2] * " (" * mu * "g m" ^-3 * ")"), 'bottomleft')
mtext("(d)", adj = 0, line=0.20, cex = 1.25)
dev.off()


# Performance

per_stats <- AllPerformaceTable(mun_obs, pin_mun_uc, "./03_output/text_files/pin_ugs_nox_x4")
per_stats[, c("COE", "IOA")]
bg_stats <- AllPerformaceTable(mun_obs, mun_bg_ibi, "./03_output/text_files/pin_bg_ugs_nox_x4")
