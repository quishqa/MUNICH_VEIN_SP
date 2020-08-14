# Export to MUNICH Background format

library(zoo)

load('02_data/bg_obs/munich_bg.rda')

ibi <- cetesb_bg[[26]]
san <- cetesb_bg[[67]]

ibi$date <- as.character(ibi$date)
san$date <- as.character(san$date)

ibi$date <- as.POSIXct(strptime(ibi$date, format = "%Y-%m-%d %H:%M"), tz = "America/Sao_Paulo")
san$date <- as.POSIXct(strptime(san$date, format = "%Y-%m-%d %H:%M"), tz = "America/Sao_Paulo")


MunichBackground <- function(df){
  bg <- data.frame(date = df$date,
                   o3 = zoo::na.approx(df$o3, na.rm = F, rule=2),
                   no2 = zoo::na.approx(df$no2, na.rm = F, rule=2),
                   no = zoo::na.approx(df$no, na.rm = F, rule=2))
  
  attributes(bg$date)$tzone <- 'UTC'
  bg$date <- format(bg$date, format = '%Y-%m-%d_%H')
  
  sta.abr <-tolower(substr(unique(na.omit(df$aqs)), 1, 3))
  sta.abr <- iconv(sta.abr, 'UTF-8', 'ASCII//TRANSLIT')
  file.name <- paste0('03_output/text_files/', sta.abr, '_bg_o3_no2_no.dat')
  
  cat("#       O3      NO2     NO\n", file = file.name)
  write.table(bg, file.name, sep = '\t', row.names = F,
              col.names = F, quote = F, append = T )
}

# MunichBackgroundFF <- function(df, ff){
#   bg <- data.frame(date = df$date,
#                    o3 = zoo::na.approx(df$o3, na.rm = F, rule=2),
#                    no2 = zoo::na.approx(df$no2, na.rm = F, rule=2),
#                    no = zoo::na.approx(df$no, na.rm = F, rule=2))
#   
#   attributes(bg$date)$tzone <- 'UTC'
#   bg$date <- format(bg$date, format = '%Y-%m-%d_%H')
#   
#   # sta.abr <-tolower(substr(unique(na.omit(df$aqs)), 1, 3))
#   # sta.abr <- iconv(sta.abr, 'UTF-8', 'ASCII//TRANSLIT')
#   file.name <- paste0('03_output/text_files/', ff, '_bg_o3_no2_no.dat')
#   
#   cat("#       O3      NO2     NO\n", file = file.name)
#   write.table(bg, file.name, sep = '\t', row.names = F,
#               col.names = F, quote = F, append = T )
# }


MunichBackground(ibi)
MunichBackground(san)

