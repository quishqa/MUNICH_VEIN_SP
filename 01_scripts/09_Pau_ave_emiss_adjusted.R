# Comparison between observation and MUNICH simulation scenario
# with adjusted emissions (x2), for 
# Paulista Avenue

source("./01_scripts/06_Post_processor_functions.R")

# Considering Pin urban canyon
pau_uc <- c(-46.652390, -23.564824)
pau_uc_sf <- st_sfc(st_point(pau_uc), crs = 4326)
ave_uc_id <- c(6759:6763, 6824:6829)
pau_coords <- "./02_data/emiss/AVPAUUGS.traf.2014100800"
coord <- read.table(pau_coords, header = T, stringsAsFactors = F)
coord <- coord[c("xa", "ya", "xb", "yb")]

ibi_bg <- LoadBackgroundUTC2LT("03_output/text_files/ibi_bg_o3_no2_no.dat")

pau_obs <- LoadObservationsLT('02_data/bg_obs/cc_oct_14.csv')

pau_ugs <- LoadMunichOutputStreet2LT("02_data/tests/av_pau_ugs_x2/", "201410",
                                        ave_uc_id)

mun_obs <- pau_obs[pau_obs$date %in% pau_ugs$date, ]
mun_bg_ibi  <- ibi_bg[ibi_bg$date %in% pau_ugs$date, ]

# Temporal Series
svg("03_output/paper_figs/Fig_11_Pau_ugs_x2.svg", height = 10, width = 14)
par(mfrow=c(2, 2), mar=c(3.2,4.6, 1.5, 0.7))
PlotTemporlPolBig(mun_obs, mun_bg_ibi, pau_ugs, 'o3', expression("O"[3] * " (" * mu * "g m" ^-3 * ")"))
mtext("(a)", adj = 0, line=0.20, cex = 1.25)
PlotTemporlPolBig(mun_obs, mun_bg_ibi, pau_ugs, 'nox', expression("NO"[X] * " (" * mu * "g m" ^-3 * ")"))
mtext("(b)", adj = 0, line=0.20, cex = 1.25)
PlotTemporlPolBig(mun_obs, mun_bg_ibi, pau_ugs, 'no', expression("NO" * " (" * mu * "g m" ^-3 * ")"))
mtext("(c)", adj = 0, line=0.20, cex = 1.25)
PlotTemporlPolBig(mun_obs, mun_bg_ibi, pau_ugs, 'no2', expression("NO"[2] * " (" * mu * "g m" ^-3 * ")"))
mtext("(d)", adj = 0, line=0.20, cex = 1.25)
dev.off()

# Daily Profile

obs.h <- HourlyProf(mun_obs)
bg.h <- HourlyProf(mun_bg_ibi)
pin.h <- HourlyProf(pau_ugs)

o3.range <- range(c(obs.h$o3, bg.h$o3, pin.h$o3), na.rm = T)
no.range <- range(c(obs.h$no, bg.h$no, pin.h$no), na.rm = T)
no2.range <- range(c(obs.h$no2, bg.h$no2, pin.h$no2), na.rm = T)
nox.range <- range(c(obs.h$nox, bg.h$nox, pin.h$nox), na.rm = T)


svg("03_output/paper_figs/Fig_12_Pau_ugs_x2.svg", width = 14, height = 10)
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


per_stats <- AllPerformaceTable(mun_obs, pau_ugs, "./03_output/text_files/avpau_ugs_x2")
per_stats[, c("COE", "IOA")]
bg_stats <- AllPerformaceTable(mun_obs, mun_bg_ibi, "./03_output/text_files/avpau_bg_ugs_x2")