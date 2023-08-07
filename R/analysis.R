library (simCAT)
library (dplyr)
# detach('package:simCAT')
# devtools::install_github('alexandrejaloto/simCAT', force = TRUE)

# prepare ----

rm(list = ls())

areas <- c('CH', 'CN', 'LC', 'MT')

source('R/conditions.R')

# which conditions exist indeed
files <- list.files('results')

conditions.exist <- c()
for(i in 1:length(conditions))
  if (sum(stringr::str_detect(files, conditions[i])) > 0)
    conditions.exist <- c(conditions.exist, conditions[i])

# table results ----

table_results <- data.frame()
table_conditional_rmse <- list()
table_conditional_se <- list()

for (area_ in areas)
{
  # area_ <- 'CH'
  table_conditional_rmse[[area_]] <- data.frame()
  table_conditional_se[[area_]] <- data.frame()

  for (i in 1:length(conditions))
  {
    # i <- 1

    if (file.exists(paste0('results/', conditions[i], '_', area_, '.RData')))
    {
      # load results
      load(paste0('results/', conditions[i], '_', area_, '.RData'))

      # load true scores
      load('rdata/samples.RData')

      # load constants
      load('rdata/official_constants.RData')

      real[[area_]] <- (real[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

      # load pars
      load('rdata/pars.RData')

      # select items
      items <- subset (pars, area == area_) %>%
        nrow()

      # item names
      items <- paste0('I', 1:items)

      # analysis
      analysis <- simCAT::cat.evaluation(
        results = results,
        true.scores = real[[area_]],
        item.name = items,
        rmax = .3
      )

      analysis

      table_results <- rbind(
        table_results,
        data.frame(
          area = area_,
          condition = conditions[i],
          selection = selection[i],
          stop = stop[i],
          NIA = analysis$evaluation['length_mean'],
          MIA = analysis$evaluation['length_median'],
          min_length = analysis$evaluation['min_length'],
          max_length = analysis$evaluation['max_length'],
          SE = analysis$evaluation['se'],
          correlation = analysis$evaluation['correlation'],
          RMSE = analysis$evaluation['rmse'],
          bias = analysis$evaluation['bias'],
          min_exp = analysis$evaluation['min_exp'],
          max_exp = analysis$evaluation['max_exp'],
          n_exp0 = analysis$evaluation['n_exp0'],
          n_exp_rmax = analysis$evaluation['n_exp_rmax'],
          overlap = analysis$evaluation['overlap']
        )
      )

      # conditional rmse
      table_conditional_rmse[[area_]] <- rbind(
        table_conditional_rmse[[area_]],
        analysis$conditional[1,]
      )

      # conditional se
      table_conditional_se[[area_]] <- rbind(
        table_conditional_se[[area_]],
        analysis$conditional[2,]
      )
    }
  }
}
rownames(table_results) <- 1:nrow(table_results)

select(table_results, -condition) %>%
  write.table(
    'results/table_results.csv',
    row.names = FALSE,
    sep = ';',
    dec = ','
  )

# summary

names(table_results)
summary_results <- table_results %>%
  group_by(area, condition) %>%
  summarise(
    length_mean_sd = sd(length_mean),
    length_mean_min = min(length_mean),
    length_mean_max = max(length_mean),
    length_mean = mean(length_mean),
    length_median_sd = sd(length_median),
    length_median_min = min(length_median),
    length_median_max = max(length_median),
    length_median = mean(length_median),
    min_length_sd = sd(min_length),
    min_length_min = min(min_length),
    min_length_max = max(min_length),
    min_length = mean(min_length),
    max_length_sd = sd(max_length),
    max_length_min = min(max_length),
    max_length_max = max(max_length),
    max_length = mean(max_length),
    se_sd = sd(se),
    se_min = min(se),
    se_max = max(se),
    se = mean(se),
    correlation_sd = sd(correlation),
    correlation_min = min(correlation),
    correlation_max = max(correlation),
    correlation = mean(correlation),
    rmse_sd = sd(rmse),
    rmse_min = min(rmse),
    rmse_max = max(rmse),
    rmse = mean(rmse),
    bias_sd = sd(bias),
    bias_min = min(bias),
    bias_max = max(bias),
    bias = mean(bias),
    min_exp_sd = sd(min_exp),
    min_exp_min = min(min_exp),
    min_exp_max = max(min_exp),
    min_exp = mean(min_exp),
    max_exp_sd = sd(max_exp),
    max_exp_min = min(max_exp),
    max_exp_max = max(max_exp),
    max_exp = mean(max_exp),
    n_exp_rmax_sd = sd(n_exp_rmax),
    n_exp_rmax_min = min(n_exp_rmax),
    n_exp_rmax_max = max(n_exp_rmax),
    n_exp_rmax = mean(n_exp_rmax),
    overlap_sd = sd(overlap),
    overlap_min = min(overlap),
    overlap_max = max(overlap),
    overlap = mean(overlap),
    n_exp0_sd = sd(n_exp0),
    n_exp0_min = min(n_exp0),
    n_exp0_max = max(n_exp0),
    n_exp0 = mean(n_exp0)
  )

write.table(
  summary_results,
  'results/table_summary_results.csv',
  row.names = FALSE,
  sep = ';',
  dec = ','
)

# conditional ----

deciles <- list()

for(area_ in areas)
  deciles[[area_]] <- as.numeric(colnames(table_conditional_rmse[[area_]]))

save(deciles, file = 'rdata/deciles.RData')

for(area_ in areas)
{

  rownames(table_conditional_rmse[[area_]]) <- conditions.exist
  rownames(table_conditional_se[[area_]]) <- conditions.exist

  names(table_conditional_se[[area_]]) <- paste0('decil', 1:10)
  names(table_conditional_rmse[[area_]]) <- paste0('decil', 1:10)

  write.table(
    table_conditional_rmse[[area_]],
    paste0('results/table_conditional_rmse_', area_, '.csv'),
    row.names = TRUE,
    sep = ';',
    dec = ','
  )

  write.table(
    table_conditional_se[[area_]],
    paste0('results/table_conditional_se_', area_, '.csv'),
    row.names = TRUE,
    sep = ';',
    dec = ','
  )
}

# exposure rates ----

exp <- list()

for(area_ in areas)
{

  # area_ <- 'CH'

  # load pars
  load('rdata/pars.RData')

  # select items
  items <- subset (pars, area == area_) %>%
    nrow()

  # item names
  items <- paste0('I', 1:items)
  exp[[area_]] <- data.frame(
    matrix(nrow = 10)
  )
  for (i in 1:length(conditions))
  {
    # i <- 1

    if (file.exists(paste0('results/', conditions[i], '_', area_, '.RData')))
    {
      load(paste0('results/', conditions[i], '_', area_, '.RData'))

      # load results
      load(paste0('results/', conditions[i], '_', area_, '.RData'))

      exposure <- data.frame(matrix(ncol = 1))
      names(exposure) <- 'items'

      for (j in 1:20)
        # j <- 1
      exposure <- dplyr::full_join(exposure,
        simCAT:::exposure.rate(results[[j]]$prev.resps, items),
        by = 'items'
      )

      exposure <- exposure[-1,]

      exposure <- data.frame(exposure$items, rowMeans(exposure[,-1]))
      names(exposure) <- c('items', 'Freq')

      exp. <- cut(
        exposure$Freq,
        c(-Inf, 0, .02, .05, .1, .15, .2, .25, .3, .4, 1)
      )
      exp. <- recode(exp., '(-Inf,0]' = '0')

      exp. <- data.frame(prop.table(table(exp.)))

      # names(exp.) <- c('exp', conditions[i])

      exp[[area_]] <- cbind(exp[[area_]], exp.[,2])
    }
  }
  names(exp[[area_]]) <- c('exp', conditions.exist)
  exp[[area_]]$exp <- exp.$exp.

}

exp

exp <- do.call(rbind, exp)

exp$area <- substr(rownames(exp), 1, 2)

exp <- select(exp, area, everything())

write.table(
  exp,
  'results/table_exp.csv',
  row.names = FALSE,
  dec = ',',
  sep = ';'
)

# end

# linear ----

load('results/linear.RData')

# load true scores
load('rdata/samples.RData')

# load constants
load('rdata/official_constants.RData')

table_linear <- data.frame()

for (area_ in areas)
{
  real[[area_]] <- (real[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

  for(i in 1:20)
    # i <- 1
    table_linear <- rbind(
      table_linear,
      data.frame(
        area = area_,
        bias = mean(linear[[area_]][[i]]$F1 - real[[area_]]),
        rmse = simCAT:::rmse (real[[area_]], linear[[area_]][[i]]$F1),
        se = mean(linear[[area_]][[i]]$SE_F1),
        correlation = cor(real[[area_]], linear[[area_]][[i]]$F1)
      )
    )
}

table_linear

write.table(
  table_linear,
  'results/table_results_linear.csv',
  row.names = FALSE,
  sep = ';',
  dec = ','
)

# summary

names(table_linear)
summary_results <- table_linear %>%
  group_by(area) %>%
  summarise(
    se_sd = sd(se),
    se_min = min(se),
    se_max = max(se),
    se = mean(se),
    correlation_sd = sd(correlation),
    correlation_min = min(correlation),
    correlation_max = max(correlation),
    correlation = mean(correlation),
    rmse_sd = sd(rmse),
    rmse_min = min(rmse),
    rmse_max = max(rmse),
    rmse = mean(rmse),
    bias_sd = sd(bias),
    bias_min = min(bias),
    bias_max = max(bias),
    bias = mean(bias)
  )

write.table(
  summary_results,
  'results/table_summary_results_linear.csv',
  row.names = FALSE,
  sep = ';',
  dec = ','
)

# conditional
load('rdata/deciles.RData')

table_linear_conditional_rmse <- list()
table_linear_conditional_se <- list()

for(area_ in areas)
{

  table_linear_conditional_rmse[[area_]] <- list()
  table_linear_conditional_se[[area_]] <- list()

  levels <- cut(x = real[[area_]], breaks = c(-Inf,deciles[[area_]]), labels = 1:10)

  for (i in 1:20)
  {
    rmse <- c()
    se <- c()

    for (q in 1:10)
    {
      rmse <- c(rmse, simCAT:::rmse(subset(linear[[area_]][[i]]$F1, levels == q), subset(real[[area_]], levels == q)))
      se <- c(se, mean(subset(linear[[area_]][[i]]$SE_F1, levels == q)))
    }

    table_linear_conditional_rmse[[area_]][[i]] <- rmse
    table_linear_conditional_se[[area_]][[i]] <- se

  }

  table_linear_conditional_rmse[[area_]] <- do.call(rbind, table_linear_conditional_rmse[[area_]])
  table_linear_conditional_rmse[[area_]] <- colMeans(table_linear_conditional_rmse[[area_]])

  table_linear_conditional_se[[area_]] <- do.call(rbind, table_linear_conditional_se[[area_]])
  table_linear_conditional_se[[area_]] <- colMeans(table_linear_conditional_se[[area_]])
}

save(table_linear_conditional_rmse, file = 'results/table_linear_conditional_rmse.RData')
save(table_linear_conditional_se, file = 'results/table_linear_conditional_se.RData')


# end

# rascunho ----

results[[1]]$prev.resps[[1]]


items <- subset (pars, area == area_)

items$CO_HABILIDADE
f <- function(x)
  table(items$CO_HABILIDADE[which(paste0("I", 1:nrow(items)) %in% x)])

f(results[[1]]$prev.resps[[1]])

lapply(results[[1]]$prev.resps, f)


f(results$prev.resps[[1]])
lapply(results$prev.resps, f)

