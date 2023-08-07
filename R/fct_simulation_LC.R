# sel.method = 'progressive'
# cat.type = 'fixed'
# acceleration = -1
# threshold = 20
# rmax = .3
# stop = list(fixed = 20)
# n = 3600
# condition = 'PR-1TF20'


fct_simulation_LC <- function(sel.method, cat.type, acceleration, threshold, rmax, stop, n, condition)
{
  area_ <- 'LC'

  load(paste0('rdata/resps_', area_, '.RData'))

  # resps[[1]] <- resps[[1]][1:10,]

  start.theta <- (m.scores[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

  items <- subset (pars, area == area_)

  results <- list()

  rep <- 1
  for (rep in 1:replications)
  {
    # rep <- 1
    print(paste0(area_, rep))

    set.seed(rep+n, sample.kind = "Rounding")

    results[[rep]] <- simCAT_LC(
      resps = resps[[rep]],
      bank = items[,1:3],
      start.theta = start.theta,
      sel.method = sel.method,
      cat.type = cat.type,
      acceleration = acceleration,
      met.weight = 'magis',
      threshold = threshold,
      rmax = rmax,
      content.names = 1:30,
      content.props = rep(1/30, 30),
      content.items = items$CO_HABILIDADE,
      met.content = "MCCAT",
      stop = stop,
      item.language = items$TP_LINGUA
    )
  }

  save(results, file = paste0('results/', condition, '_', area_, '.RData'))

}
