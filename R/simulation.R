# detach('package:simCAT')
# devtools::install_github('alexandrejaloto/simCAT')
library (simCAT)
library (dplyr)

rm(list = ls())

# prepare simulation ----

# load parameters
load('rdata/pars.RData')

# load Enem mean
load('rdata/mean.RData')

# load constants
load('rdata/official_constants.RData')

# areas <- c('CH', 'CN', 'LC', 'MT')
areas <- c('CH', 'CN', 'MT')
# areas <- c('CN', 'MT')
# areas <- c('CH')
replications <- 20

source('R/fct_simulation.R')

# end

# selection: random; fixed length (45) ----

fct_simulation(
  sel.method = 'random',
  cat.type = 'fixed',
  acceleration = 1,
  threshold = 45,
  rmax = 1,
  stop = list(fixed = 45),
  n = 0,
  condition = 'ALETF45'
)

# selection: random; fixed length (20) ----

fct_simulation(
  sel.method = 'random',
  cat.type = 'fixed',
  acceleration = 1,
  threshold = 20,
  rmax = 1,
  stop = list(fixed = 20),
  n = 400,
  condition = 'ALETF20'
)

# selection: random; standard error (0.3) ----

fct_simulation(
  sel.method = 'random',
  cat.type = 'variable',
  acceleration = 1,
  threshold = .3,
  rmax = 1,
  stop = list(se = .3, min.items = 15, max.items = 60),
  n = 800,
  condition = 'ALEEP30'
)

# selection: random; standard error (.3)/error reduction (.015) ----

fct_simulation(
  sel.method = 'random',
  cat.type = 'variable',
  acceleration = 1,
  threshold = .3,
  rmax = 1,
  stop = list(se = .3, min.items = 15, max.items = 60, hypo = .015, hyper = Inf),
  n = 1200,
  condition = 'ALEEP30RE015'
)

# selection: MFI; fixed length (45) ----

fct_simulation(
  sel.method = 'MFI',
  cat.type = 'fixed',
  acceleration = 1,
  threshold = 45,
  rmax = 1,
  stop = list(fixed = 45),
  n = 1600,
  condition = 'MIFTF45'
)

# selection: MFI; fixed length (20) ----

fct_simulation(
  sel.method = 'MFI',
  cat.type = 'fixed',
  acceleration = 1,
  threshold = 20,
  rmax = 1,
  stop = list(fixed = 20),
  n = 2000,
  condition = 'MIFTF20'
)

# selection: MFI; standard error (0.3) ----

fct_simulation(
  sel.method = 'MFI',
  cat.type = 'variable',
  acceleration = 1,
  threshold = .3,
  rmax = 1,
  stop = list(se = .3, min.items = 15, max.items = 60),
  n = 2400,
  condition = 'MIFEP30'
)

# selection: MFI; standard error (.3)/error reduction (.015) ----

fct_simulation(
  sel.method = 'MFI',
  cat.type = 'variable',
  acceleration = 1,
  threshold = .3,
  rmax = 1,
  stop = list(se = .3, min.items = 15, max.items = 60, hypo = .015, hyper = Inf),
  n = 2800,
  condition = 'MIFEP30RE015'
)

# selection: PR (acceleration parameter = 1); fixed length (45) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'fixed',
  acceleration = 1,
  threshold = 45,
  rmax = .3,
  stop = list(fixed = 45),
  n = 3200,
  condition = 'PR1TF45'
)

# selection: PR (acceleration parameter = 1); fixed length (20) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'fixed',
  acceleration = 1,
  threshold = 20,
  rmax = .3,
  stop = list(fixed = 20),
  n = 3600,
  condition = 'PR1TF20'
)

# selection: PR (acceleration parameter = 1); standard error (0.3) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'variable',
  acceleration = 1,
  threshold = .3,
  rmax = .3,
  stop = list(se = .3, min.items = 15, max.items = 60),
  n = 4000,
  condition = 'PR1EP30'
)

# selection: PR (acceleration parameter = 1); standard error (.3)/error reduction (.015) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'variable',
  acceleration = 1,
  threshold = .3,
  rmax = .3,
  stop = list(se = .3, min.items = 15, max.items = 60, hypo = .015, hyper = Inf),
  n = 4400,
  condition = 'PR1EP30RE015'
)


# selection: PR (acceleration parameter = 2); fixed length (45) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'fixed',
  acceleration = 2,
  threshold = 45,
  rmax = .3,
  stop = list(fixed = 45),
  n = 3200,
  condition = 'PR2TF45'
)

# selection: PR (acceleration parameter = 2); fixed length (20) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'fixed',
  acceleration = 2,
  threshold = 20,
  rmax = .3,
  stop = list(fixed = 20),
  n = 3600,
  condition = 'PR2TF20'
)

# selection: PR (acceleration parameter = 2); standard error (0.3) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'variable',
  acceleration = 2,
  threshold = .3,
  rmax = .3,
  stop = list(se = .3, min.items = 15, max.items = 60),
  n = 4000,
  condition = 'PR2EP30'
)

# selection: PR (acceleration parameter = 2); standard error (.3)/error reduction (.015) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'variable',
  acceleration = 2,
  threshold = .3,
  rmax = .3,
  stop = list(se = .3, min.items = 15, max.items = 60, hypo = .015, hyper = Inf),
  n = 4400,
  condition = 'PR2EP30RE015'
)


# selection: PR (acceleration parameter = 3); fixed length (45) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'fixed',
  acceleration = 3,
  threshold = 45,
  rmax = .3,
  stop = list(fixed = 45),
  n = 4800,
  condition = 'PR3TF45'
)

# selection: PR (acceleration parameter = 3); fixed length (20) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'fixed',
  acceleration = 3,
  threshold = 20,
  rmax = .3,
  stop = list(fixed = 20),
  n = 5200,
  condition = 'PR3TF20'
)

# selection: PR (acceleration parameter = 3); standard error (0.3) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'variable',
  acceleration = 3,
  threshold = .3,
  rmax = .3,
  stop = list(se = .3, min.items = 15, max.items = 60),
  n = 5600,
  condition = 'PR3EP30'
)

# selection: PR (acceleration parameter = 3); standard error (.3)/error reduction (.015) ----

fct_simulation(
  sel.method = 'progressive',
  cat.type = 'variable',
  acceleration = 3,
  threshold = .3,
  rmax = .3,
  stop = list(se = .3, min.items = 15, max.items = 60, hypo = .015, hyper = Inf),
  n = 6000,
  condition = 'PR3EP30RE015'
)

