# NOX test
# A new background test using San

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

pin_mun_uc <- LoadMunichOutputStreet2LT("02_data/tests/pin_ugs_x2/", "201410",
                                         Links2Point(pin_coords_file, pin_est, 25))

mun_obs <- pin_obs[pin_obs$date %in% pin_mun_uc$date, ]
mun_bg_ibi  <- ibi_bg[ibi_bg$date %in% pin_mun_uc$date, ]

# Temporal Series
svg("03_output/paper_figs/Fig_08_Pin_ugs_x2.svg", height = 10, width = 14)
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


svg("03_output/paper_figs/Fig_09_Pin_ugs_x2.svg", width = 14, height = 10)
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

per_stats <- AllPerformaceTable(mun_obs, pin_mun_uc, "./03_output/text_files/pin_ugs_x2")
per_stats[, c("COE", "IOA")]
bg_stats <- AllPerformaceTable(mun_obs, mun_bg_ibi, "./03_output/text_files/pin_bg_ugs_x2")

# mean uc concentration

pin_nb <- LoadMunResAll("02_data/tests/pin_ugs_x2/", '201410')
pin_nb_mean <- Reduce('+', pin_nb) /length(pin_nb)
pin_nb_sf <- MunOutput2Sf(coord, pin_nb_mean)

light <- c(-46.694405, -23.565093)
light_sf <- st_sfc(st_point(light), crs = 4326)



library(gridExtra)
o3_st <- ggplot()+
  geom_sf(data=pin_nb_sf['o3'], aes(col=o3), lwd = 1.25)+
  geom_sf(data=pin_est_sf, size = 3, shape = 23, fill = 'Red' )+
  geom_sf(data=light_sf, size = 3, shape = 23, fill = 'orange' )+
  scale_colour_viridis_c(name=expression("(" * mu * "g m" ^-3 * ")"),
                         option = 'D', direction = -1) +
  theme_bw(base_size = 17 ) +
  ggtitle(expression("O"[3]), subtitle = 'Pinheiros') + labs(tag = "(a)")

nox_st <-  ggplot()+
  geom_sf(data=pin_nb_sf['nox'], aes(col=nox), lwd = 1.25)+
  geom_sf(data=pin_est_sf, size = 3, shape = 23, fill = 'Red' )+
  geom_sf(data=light_sf, size = 3, shape = 23, fill = 'orange' )+
  scale_colour_viridis_c(name=expression("(" * mu * "g m" ^-3 * ")"),
                         option = 'D', direction = -1) +
  theme_bw(base_size = 17) +
  ggtitle(expression("NO"[X]), subtitle = 'Pinheiros')  + labs(tag = "(b)")

svg("03_output/paper_figs/Fig_10_st.svg", width = 15, height = 7)
grid.arrange(o3_st, nox_st, ncol=2)
dev.off()



mean(pin_mun_uc$o3 - mun_bg_ibi$o3, na.rm = T)
mean(pin_mun_uc$nox - mun_bg_ibi$nox, na.rm = T)
mean(pin_mun_uc$no - mun_bg_ibi$no, na.rm = T)
mean(pin_mun_uc$no2 - mun_bg_ibi$no2, na.rm = T)