# Downloading observation from PInheiros and Cerqueira
# Cesar air quality stations (AQS) for model evaluation.
# And Downloading concentration from Ibirapuera AQS 
# as MUNICH background concentration.

#devtools::install_github("quishqa/qualR")
library(qualR)

user <- "xxxxxxxx"
pass <- "xxxxxxx"
start <- "01/10/2014"
end <- "31/10/2014"


cetesb <- cetesb_aqs


MunichBakgroundRaw <- function(code_aqs){
  o3  <- CetesbRetrieve(user, pass, 63, 
                        code_aqs, start, end)
  no2 <- CetesbRetrieve(user, pass, 15, 
                        code_aqs, start, end)
  no  <- CetesbRetrieve(user, pass, 17, 
                        code_aqs, start, end)
  
  bg <- data.frame(date = o3$date,
                   o3 = o3$pol,
                   no2 = no2$pol,
                   no = no$pol,
                   aqs = o3$aqs,
                   stringsAsFactors = F)
  print(
    paste0("Station: ", unique(bg$aqs))
  )
  return(bg)
}


# # Download all AQS stations.
# cetesb_bg <- lapply(cetesb$code, MunichBakgroundRaw)
# 
# save(cetesb_bg, file = "./02_data/munich_bg.rda")


# Downloading observations ------------------------------------------------

pin_obs <- CetesbRetrievePol(user, pass, 99, start, end)
pin_obs_csv <- pin_obs[c('date', 'o3', 'no', 'no2')]
write.table(pin_obs_csv, file = './02_data/bg_obs/pin_oct_14.csv', sep = ',', 
            row.names = F, quote = F)

cc_obs <- CetesbRetrievePol(user, pass, 91, start, end)
cc_obs_csv <- cc_obs[c('date', 'o3', 'no', 'no2')]
write.table(cc_obs_csv, file = './02_data/bg_obs/cc_oct_14.csv', sep = ',', 
            row.names = F, quote = F)
