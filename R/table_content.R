library (simCAT)
library (dplyr)
library(tidyverse)

rm(list = ls())

areas <- c('CH', 'CN', 'LC', 'MT')

# load pars
load('rdata/pars.RData')

table_content <- data.frame(
  habilidade = 1:30
)

for (area_ in areas)
{
  # area_ <- 'CH'

  load(paste0('results/PR2TF20', '_', area_, '.RData'))

  pars_ <- subset(pars, area == area_)

  item.resp <- function(x)
    as.numeric(substr(x, 2, 5))

  prop <- data.frame(matrix(nrow = 30))

  for(j in 1:20)
  {
    admin.items <- lapply(results[[j]]$prev.resps, item.resp)

    admin.items <- do.call(c, admin.items)

    admin.items <- data.frame(
      item = admin.items,
      habilidade = pars_$CO_HABILIDADE[admin.items]
    )

    table(admin.items$item)

    prop[,j] <- as.numeric(table(admin.items$habilidade))/nrow(results[[1]]$score)

  }
    # table_content[paste0('prop_', area_)] <- as.numeric(table(admin.items$habilidade))/nrow(results[[1]]$score)

    table_content[paste0('prop_', area_)] <- rowMeans(prop)

    table_content[paste0('itens_', area_)] <- table(pars_$CO_HABILIDADE)

}

a_content <- pars %>%
  group_by(area, CO_HABILIDADE) %>%
  summarise(mean_a = mean(NU_PARAM_A)) %>%
  pivot_wider(names_from = area, values_from = mean_a, names_prefix = 'mean_a_') %>%
  data.frame()

table_content <- inner_join(table_content, a_content, by = c('habilidade' = 'CO_HABILIDADE'))

table_content <- select(table_content, habilidade, paste0(c('itens_', 'mean_a_', 'prop_'), rep(areas, each = 3)))

table_content

# fit <- lm(prop_CH ~ itens_CH + mean_a_CH, data = table_content)
# summary(fit)
# fit <- lm(prop_CN ~ itens_CN + mean_a_CN, data = table_content)
# summary(fit)
# fit <- lm(prop_LC ~ itens_LC + mean_a_LC, data = table_content)
# summary(fit)
# fit <- lm(prop_MT ~ itens_MT + mean_a_MT, data = table_content)
# summary(fit)

write.table(
  table_content,
  'results/table_content.csv',
  row.names = FALSE,
  sep = ';',
  dec = ','
)

