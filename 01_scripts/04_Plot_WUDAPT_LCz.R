# Plotting WUDAPT LCZ for Sao Paulo Metropolitan Area
# in a lat and lon projection.

library(raster)
library(viridis)
library(rasterVis)
library(dplyr)

wu_geo <- raster("02_data/wudapt_lat_lon.tif")

wu_geo_r <- ratify(wu_geo)
# levelplot(wu_geo)

# Building height for each LCZ
lcz <- data.frame(lcz = c(1:10, c(101:107)), 
                  name = paste0("LCZ", c(1:10, LETTERS[1:7])))

rat <- levels(wu_geo_r)[[1]]
rat <- rat %>% left_join(lcz, by = c("ID" = "lcz"))
levels(wu_geo_r) <- rat

pdf("03_output/test_figs/Fig_03_wudapt_lcz_vert.pdf")
mapTheme <- rasterTheme(region = inferno(17, direction = -1))
levelplot(wu_geo_r, ylab = "", xlab ="", par.setting=mapTheme)
dev.off()

pdf("03_output/paper_figs/Fig_03_wudapt_lcz.pdf")
levelplot(wu_geo_r, ylab = "", xlab ="", par.setting=mapTheme, axes=T,
          colorkey = list(space ="bottom",height=1, width=1,
                          labels = list(cex = 0.7)))
dev.off()