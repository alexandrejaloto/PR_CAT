library(dplyr)
library(ggplot2)
rm(list = ls())


# preparation ----

areas <- c('MT')

# conditions
source('R/conditions.R')

# which conditions exist indeed
files <- list.files('results')

conditions.exist <- c()
for(i in 1:length(conditions))
  if (sum(stringr::str_detect(files, conditions[i])) > 0)
    conditions.exist <- c(conditions.exist, conditions[i])

# load deciles
load('rdata/deciles.RData')

# load parameters
load('rdata/pars.RData')

# load constants
load('rdata/official_constants.RData')

# load samples
load('rdata/samples.RData')

# precision ----

table_conditional_rmse <- read.table(
  'results/table_conditional_rmse_MT.csv',
  header = TRUE,
  sep = ';',
  dec = ','
)

table_conditional_se <- read.table(
  'results/table_conditional_se_MT.csv',
  header = TRUE,
  sep = ';',
  dec = ','
)

graphic <- data.frame()

for(i in 1:length(conditions.exist))
  graphic <- rbind(
    graphic,
    data.frame(
      x = deciles[['MT']],
      y = as.numeric(table_conditional_rmse[i,]),
      condition = conditions.exist[i],
      graph = 'RMSE'
    )
  )

for(i in 1:length(conditions.exist))
  graphic <- rbind(
    graphic,
    data.frame(
      x = deciles[['MT']],
      y = as.numeric(table_conditional_se[i,]),
      condition = conditions.exist[i],
      graph = 'Standard Error'
    )
  )

graphic$selection.met <- substr(graphic$condition, 1, 3)

recode.selection <- c(
  'Random',
  'Maximum Fisher Information',
  'Progressive-restricted (1)',
  'Progressive-restricted (2)',
  'Progressive-restricted (3)'
)

names(recode.selection) <- name_selection

graphic$selection.met <- recode(graphic$selection.met, !!!recode.selection)

graphic$stop.crit <- factor(
  substr(graphic$condition, 4, 12),
  levels = c('TF20', 'TF45', 'EP30', 'EP30RE015')
)

recode.stop <- c('FL20', 'FL45', 'SE30', 'SE30ER015')
names(recode.stop) <- c('TF20', 'TF45', 'EP30', 'EP30RE015')
graphic$stop.crit <- recode(graphic$stop.crit, !!!recode.stop)

p <- graphic %>%
  ggplot(aes(x = x, y = y, shape = selection.met, colour = selection.met, linetype = selection.met)) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(stop.crit), rows = vars(graph)) +
  scale_colour_discrete(name = 'Selection') +
  scale_linetype_discrete(name = 'Selection') +
  scale_shape_discrete(name = 'Selection') +
  labs(x="theta", y = '') +
  theme_bw() +
  theme(legend.position = 'bottom')
p

png (
  filename = paste0 ('graphics/precision_MT.png'),
  width = 5000,
  height = 3200,
  units = "px",
  pointsize = 12,
  bg = "white",
  res = 500,
  restoreConsole = TRUE
)

p

dev.off()

# density and information ----

thetas.sample <- list()
for(area_ in areas)
{
  thetas.sample[[area_]] <- (real[[area_]] - official.constants[[area_]]$m)/official.constants[[area_]]$s

  thetas.sample[[area_]] <- data.frame(thetas.sample[[area_]]) %>%
    mutate(area = area_) %>%
    rename(theta = thetas.sample..area_..)
}

thetas.sample <- do.call(rbind, thetas.sample)

max.theta <- max(thetas.sample$theta)

thetas <- seq (-3, max(max.theta), .01)

info.graphic <- list()
# simCAT::calc.info()
for (area_ in areas)
{
  items <- subset (pars, area == area_)
  info.graphic[[area_]] <- lapply(thetas, function(x) sum(simCAT::calc.info(items, x))) %>%
    do.call(rbind, .) %>%
    data.frame()
  info.graphic[[area_]]$area <- area_
}

info.graphic <- do.call(rbind, info.graphic)
names(info.graphic)[1] <- 'info'
info.graphic$thetas.info <- thetas

recode.area <- c('HS', 'NS', 'LC', 'MT')
names(recode.area) <- c('CH', 'CN', 'LC', 'MT')
thetas.sample$area <- recode(thetas.sample$area, !!!recode.area)
info.graphic$area <- recode(info.graphic$area, !!!recode.area)

png (
  filename = paste0 ('graphics/density_information_MT.png'),
  width = 3200,
  height = 3200,
  units = "px",
  pointsize = 12,
  bg = "white",
  res = 500,
  restoreConsole = TRUE
)

ggplot() +
  geom_density(data = thetas.sample, aes_string(thetas.sample$theta)) +
  geom_line(data = info.graphic, aes(x = thetas.info, y = info/(max(info)/.5)), linetype = 2) +
  scale_y_continuous(sec.axis = sec_axis(~./(max(info.graphic$info)/.5), name = 'information')) +
  labs(x= "theta", y = "density") +
  theme_bw()

dev.off()

# end

# linear ----


table_conditional_rmse <- data.frame()

for(area_ in areas)
  table_conditional_rmse <- rbind(
    table_conditional_rmse,
    data.frame(
      read.table(
        paste0('results/table_conditional_rmse_', area_, '.csv'),
        header = TRUE,
        sep = ';',
        dec = ','
      ),
      area = area_
    )
  )

table_conditional_se <- data.frame()

for(area_ in areas)
  table_conditional_se <- rbind(
    table_conditional_se,
    data.frame(
      read.table(
        paste0('results/table_conditional_se_', area_, '.csv'),
        header = TRUE,
        sep = ';',
        dec = ','
      ),
      area = area_
    )
  )

graphic <- data.frame()

for(area_ in areas)
{
  # area_ <- 'CH'
  graphic. <- subset(table_conditional_rmse, area == area_)[,-11]

  graphic <- rbind(
    graphic,
    data.frame(
      x = deciles[[area_]],
      y = as.numeric(graphic.['PR2TF20',]),
      condition = 'PR2TF20',
      precision = 'REQM',
      area = area_
    )
  )
}

for(area_ in areas)
{
  # area_ <- 'CH'
  graphic. <- subset(table_conditional_se, area == area_)[,-11]

  graphic <- rbind(
    graphic,
    data.frame(
      x = deciles[[area_]],
      y = as.numeric(graphic.['PR2TF20',]),
      condition = 'PR2TF20',
      precision = 'Erro',
      area = area_
    )
  )
}

load('results/table_linear_conditional_rmse.RData')
load('results/table_linear_conditional_se.RData')

for(area_ in areas)
{
  # area_ <- 'CH'
  # graphic. <- subset(table_conditional_rmse, area == area_)[,-11]

  graphic <- rbind(
    graphic,
    data.frame(
      x = deciles[[area_]],
      y = table_linear_conditional_rmse[[area_]],
      condition = 'Linear',
      precision = 'REQM',
      area = area_
    )
  )
}

for(area_ in areas)
{
  # area_ <- 'CH'
  # graphic. <- subset(table_conditional_rmse, area == area_)[,-11]

  graphic <- rbind(
    graphic,
    data.frame(
      x = deciles[[area_]],
      y = table_linear_conditional_se[[area_]],
      condition = 'Linear',
      precision = 'Erro',
      area = area_
    )
  )
}

graphic$condition <- ifelse(graphic$condition == 'PR2TF20', 'PR2FL20', graphic$condition)
graphic$precision <- ifelse(graphic$precision == 'Erro', 'Standard error', 'RMSE')

recode.area <- c('HS', 'NS', 'LC', 'MT')
names(recode.area) <- c('CH', 'CN', 'LC', 'MT')

graphic$area <- recode(graphic$area, !!!recode.area)

p <- graphic %>%
  ggplot(aes(x = x, y = y, shape = condition, colour = condition, linetype = condition)) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(precision)) +
  scale_colour_discrete(name = 'Condition') +
  scale_linetype_discrete(name = 'Condition') +
  scale_shape_discrete(name = 'Condition') +
  labs(x="theta", y = '') +
  theme_bw() +
  theme(legend.position = 'bottom')

p

png (
  filename = paste0 ('graphics/linear_MT.png'),
  width = 5000,
  height = 3200,
  units = "px",
  pointsize = 12,
  bg = "white",
  res = 500,
  restoreConsole = TRUE
)

p

dev.off()


# rascunho ----

# function to plot
plot.cond <- function (data, title)
{
  ggplot(data, aes(x = x, y = y)) +
    geom_point(aes(shape = condition, colour = condition)) +
    geom_line(aes(linetype = condition, colour = condition)) +
    ylim(y.min, y.max) +
    scale_colour_discrete(labels=selection, name = 'Seleção') +
    scale_linetype_discrete(labels = selection, name = 'Seleção') +
    scale_shape_discrete(labels = selection, name = 'Seleção') +
    labs(title=title, x="teta", y = label.y) +
    theme_bw()
}

y.min <- min(graphic$y)
y.max <- max(graphic$y)

selection <- c('Aleatório', 'Máxima Informação de Fisher', 'Progressivo Restrito (-1)', 'Progressivo Restrito (0)', 'Progressivo Restrito (1)', 'Progressivo Restrito (2)')
label.y = 'REQM'


p <- list()

# fixed length (45)
p[[1]] <- plot.cond(
  data = graphic[stringr::str_detect(graphic$condition, 'TF45'),],
  title = 'Tamanho fixo (45)'
)

# fixed length (20)
p[[2]] <- plot.cond(
  graphic[stringr::str_detect(graphic$condition, 'TF20'),],
  title = 'Tamanho fixo (20)'
)

# Standard error (0.30)
p[[3]] <- plot.cond(
  graphic[stringr::str_detect(graphic$condition, 'EP30') & !(stringr::str_detect(graphic$condition, 'RE015')),],
  title = 'Erro padrão (0,30)'
)

# Standard error (0.30) or error reduction (0.015)
p[[4]] <- plot.cond(
  graphic[stringr::str_detect(graphic$condition, 'RE015'),],
  title = 'Erro padrão (0,30) ou Redução do erro (0,015)'
)

cowplot::plot_grid(
  p[[1]],
  p[[2]],
  p[[3]],
  p[[4]]
)


p
