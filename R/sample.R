library (data.table)
library (dplyr)

rm(list = ls())
gc()

# load parameters
load('rdata/pars.RData')

data <- fread (
  'D:/Microdados/2020/MICRODADOS_ENEM_2020.csv',
  # nrow = 30000,
  select = c(
    'TP_LINGUA',
    paste0(
      'NU_NOTA_',
      rep (c('CH', 'CN', 'LC', 'MT')
      )
    )
  )
)

scores <- list()
areas <- c ('CH', 'CN', 'MT')

# filter who answered at least one item
for (area_ in areas)
{
  scores[[area_]] <- data[,get (paste0('NU_NOTA_', area_))] %>%
    subset (. > 0) %>%
    data.frame()
}

# draw simple random sample from Enem 2020
# sample error = 5
alpha <- .05
real <- list()

Z <- qnorm(1-(alpha/2))

error <- 5

for (area_ in areas)
{

  names (scores[[area_]]) <- 'scores'

  # variance
  S <- var(scores[[area_]])
  sd <- sd (scores[[area_]]$scores)

  n.items <- subset (pars, area == area_) %>%
    nrow()

  n <- max(
    ceiling ((sd*Z/error)^2),
    3*n.items
  )

  set.seed(1)
  real[[area_]] <- sample(scores[[area_]]$scores, n)

}


# LC ----
area_ <- 'LC'

data.lc <- select(data, NU_NOTA_LC, TP_LINGUA) %>%
  subset (NU_NOTA_LC > 0) %>%
  data.frame()

scores[['LC']] <- data.lc['NU_NOTA_LC']

names (scores[[area_]]) <- 'scores'

# variance
S <- var(data.lc$NU_NOTA_LC)
sd <- sd (data.lc$NU_NOTA_LC)

n.items <- subset (pars, area == area_) %>%
  nrow()

n <- max(
  ceiling ((sd*Z/error)^2),
  5*n.items
)

set.seed(1)
sample.lc <- sample(nrow(data.lc), n)

language <- data.lc$TP_LINGUA[sample.lc]
real[[area_]] <- data.lc$NU_NOTA_LC[sample.lc]

save(real, file = 'rdata/samples.RData')
save(language, file = 'rdata/language.RData')

lapply (real, length)
lapply (scores, nrow)

lapply (scores, var)

sd. <- function (x)
{
  unlist (x) %>%
    sd()
}

lapply (scores, sd.)
lapply (real, sd.)

lapply (real, summary)
lapply (scores, summary)

description <- data.frame()
for(area_ in c('CH', 'CN', 'LC', 'MT'))
  description <- rbind(
    description,
    data.frame(
      area = area_,
      samp_n = length(real[[area_]]),
      samp_m = mean(real[[area_]]),
      samp_sd = sd(real[[area_]]),
      samp_min = min(real[[area_]]),
      samp_max = max(real[[area_]]),
      pop_n = nrow(scores[[area_]]),
      pop_m = mean(scores[[area_]]$scores),
      pop_sd = sd(scores[[area_]]$scores),
      pop_min = min(scores[[area_]]$scores),
      pop_max = max(scores[[area_]]$scores)
    )
  )
description

write.table(
  description,
  'results/sample_description.csv',
  dec = ',',
  sep = ';',
  row.names = FALSE
)

m.scores <- lapply(scores, function(x) mean(pull(select(x, scores))))
save(m.scores, file = 'rdata/mean.RData')

# metric (0,1)
load('rdata/official_constants.RData')

areas <- c('CH', 'CN', 'LC', 'MT')

real2 <- list()
for (area_ in areas)
  real2[[area_]] <- (real[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

scores2 <- list()
for (area_ in areas)
  scores2[[area_]] <- (scores[[area_]]$scores - official.constants[[area_]]$m)/official.constants[[area_]]$s

description <- data.frame()
for(area_ in c('CH', 'CN', 'LC', 'MT'))
  description <- rbind(
    description,
    data.frame(
      area = area_,
      samp_n = length(real[[area_]]),
      samp_m = mean(real2[[area_]]),
      samp_sd = sd(real2[[area_]]),
      samp_min = min(real2[[area_]]),
      samp_max = max(real2[[area_]]),
      pop_n = nrow(scores[[area_]]),
      pop_m = mean(scores2[[area_]]),
      pop_sd = sd(scores2[[area_]]),
      pop_min = min(scores2[[area_]]),
      pop_max = max(scores2[[area_]])
    )
  )

write.table(
  description,
  'results/sample_description_01.csv',
  dec = ',',
  sep = ';',
  row.names = FALSE
)

official.constants
