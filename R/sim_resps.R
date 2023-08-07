library (simCAT)
library (dplyr)

rm(list = ls())
gc()

# load parameters
load('rdata/pars.RData')

# load thetas (samples)
load('rdata/samples.RData')

# load constants
load('rdata/official_constants.RData')

names(pars)
areas <- c('CH', 'CN', 'LC', 'MT')
# areas <- c('LC', 'MT')
replication <- 30

for(area_ in areas)
{

  items <- subset (pars, area == area_) %>%
    select (NU_PARAM_A, NU_PARAM_B, NU_PARAM_C)

  thetas <- (real[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

  resps <- list()

  set.seed(1000)
  for(rep in 1:replication)
  {
    print(paste0(area_, rep))
    resps[[rep]] <- data.frame(simCAT::gen.resp(thetas, items))
  }
  save(resps, file = paste0('rdata/resps_', area_, '.RData'))
}

