library (dplyr)
library (tidyr)

rm(list = ls())

# testlets codes
source('R/testlets.R')

pars <- data.frame()

areas <- c('CH', 'CN', 'LC', 'MT')
# area <- 'CH'
# year <- 2020
# application <- 1
for (area in areas)
{
  for (year in 2009:2020)
  {
    # import official parameters
    items <- read.table(
      paste0('items/ITENS_PROVA_', year, '.csv'),
      sep = ';',
      header = TRUE
    )

    for(application in 1:2)
    {
      if(year == 2009)
        {
        # filter items of each area
        items_area <- subset(items, SG_AREA = area) %>%
          # filter first testlet
          subset(CO_PROVA == testlets[[paste0('year.', year, '.', application)]][[area]][1]) %>%
          # select parameters and content
          select(starts_with('NU_PAR'), CO_HABILIDADE, CO_ITEM) %>%
          # exclude items without parameters
          drop_na()

        if(nrow(items_area) > 0)
      items_area$TP_LINGUA <- NA

      } else {

      # filter items of each area
      items_area <- subset(items, SG_AREA = area) %>%
        # filter first testlet
        subset(CO_PROVA == testlets[[paste0('year.', year, '.', application)]][[area]][1]) %>%
        # select parameters and content
        select(starts_with('NU_PAR'), CO_HABILIDADE, TP_LINGUA, CO_ITEM) %>%
        # exclude items without parameters
        drop_na(starts_with('NU_PAR'), CO_HABILIDADE)

      }

      if(nrow(items_area) > 0)
      {
        items_area$area <- area
        items_area$year <- year
        items_area$application <- application

        pars <- rbind(
          pars,
          items_area
        )
      }

      if(application == 2 & year %in% c(2016, 2020))
      {
        # filter items of each area
        items_area <- subset(items, SG_AREA = area) %>%
          # filter first testlet
          subset(CO_PROVA == testlets[[paste0('year.', year, '.', application)]][[area]][1]) %>%
          # select parameters and content
          select(starts_with('NU_PAR'), CO_HABILIDADE, TP_LINGUA, CO_ITEM) %>%
          # exclude items without parameters
          drop_na(starts_with('NU_PAR'), CO_HABILIDADE)

        items_area$area <- area
        items_area$year <- year
        items_area$application <- 3

        pars <- rbind(
          pars,
          items_area
        )
      }
    }
  }
}

# a <- subset(pars, CO_ITEM %in% pars$CO_ITEM[duplicated(pars$CO_ITEM)]) %>%
#   arrange(CO_ITEM)

pars <- pars[!duplicated(pars$CO_ITEM),]

table(pars$area)

save(pars, file = 'rdata/pars.RData')

# description ----

library (dplyr)

load('rdata/pars.RData')

pars

areas <- c('CH', 'CN', 'LC', 'MT')

pars %>%
  group_by(area) %>%
  summarise(
    mean.a = mean(NU_PARAM_A),
    mean.b = mean(NU_PARAM_B),
    mean.c = mean(NU_PARAM_C),
    sd.a = sd(NU_PARAM_A),
    sd.b = sd(NU_PARAM_B),
    sd.c = sd(NU_PARAM_C),
    n = n()
  ) %>%
  write.table(
    'results/pars.csv',
    row.names = FALSE,
    sep = ';',
    dec = ','
  )

