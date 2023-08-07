library(simCAT)
library(dplyr)
library(mirtCAT)

rm(list = ls())

# load parameters
load('rdata/pars.RData')

areas <- c('CH', 'CN', 'LC', 'MT')

linear <- list()

for(area_ in areas)
{
  # area_ <- 'LC'

  # items from area
  items.area <- subset(pars, area == area_)
  items.area$item <- 1:nrow(items.area)

  # only 1st application from 2020
  items.area <- subset(items.area, year == 2020 & application == 1)

  # load responses
  load(paste0('rdata/resps_', area_, '.RData'))

  # mirt object
  mod <- items.area %>%
    select(starts_with('NU_PAR')) %>%
    rename(a1 = 'NU_PARAM_A', d = 'NU_PARAM_B', g = 'NU_PARAM_C') %>%
    mutate(d = -a1*d) %>%
    generate.mirt_object('3PL')

  if(area_ == 'LC')
  {
    lang0 <- items.area$item[which(items.area$TP_LINGUA == 0)]
    lang1 <- items.area$item[which(items.area$TP_LINGUA == 1)]

    load(paste0('rdata/language.RData'))
    # i <- 1
    for(i in 1:20)
    {
      resps[[i]][which(language == 0),lang1] <- NA
      resps[[i]][which(language == 1),lang0] <- NA
    }
  }

  linear[[area_]] <- list()

  for(i in 1:20)
  {
    linear[[area_]][[i]] <- fscores(
      mod,
      response.pattern = resps[[i]][,items.area$item],
      quadpts = 40,
      theta_lim = c(-4, 4)
    ) %>%
      data.frame()
  }
}

names(linear)
save(linear, file = 'results/linear.RData')

# rascunho ----

theta <- apply(
  resps[[1]][,items.area$item],
  1,
  eap,
  items.area[,1:3]
)

eap(
  resps[[1]][3,items.area$item],
  items.area[,1:3]
)


inicio <- Sys.time()
theta2 <- apply(
  resps[[1]][1,items.area$item],
  1,
  eap,
  items.area[,1:3]
)
fim <- Sys.time()
fim-inicio

all.equal(
  data.frame(theta1),
  do.call(rbind, theta2)
)
theta2

nrow(resps[[1]]) * (fim-inicio) / 600

# load constants
load('rdata/official_constants.RData')

# areas <- c('CH', 'CN', 'LC', 'MT')
areas <- c('CH', 'CN', 'MT')
# areas <- c('CN', 'MT')
# areas <- c('CH')
replications <- 1
