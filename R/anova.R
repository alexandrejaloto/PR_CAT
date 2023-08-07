library (dplyr)
library (car)
library (rstatix)

rm(list = ls())

areas <- c ('CH', 'CN', 'LC', 'MT')
table_results <- read.table('results/table_results.csv',
                            sep = ';',
                            dec = ',',
                            header = TRUE
)

table_results$selection <- factor(table_results$selection, levels = c('Aleatorio', 'MIF', 'PR-1', 'PR0', 'PR1', 'PR2'))
table_results$stop <- factor(table_results$stop, levels = c('TF45', 'TF20', 'EP30', 'EP30RE015'))

# anova assumptions ----

assumption <- list()
for (area_ in areas)
{
  # area_ <- 'CH'
  assumption[[area_]] <- list()

  # leveneTest(RMSE ~ stop , data = table_results)
  # homoscedasticity
  levene_selection <- table_results %>%
    filter (area == area_) %>%
    leveneTest(RMSE ~ selection, data = .)

  levene_stop <- table_results %>%
    filter (area == area_) %>%
    leveneTest(RMSE ~ stop, data = .)

  # normality
  model <- table_results %>%
    filter (area == area_) %>%
    lm(RMSE ~ stop + selection, data = .)

  shapiro <- shapiro_test(residuals(model))
  assumption[[area_]] <- (levene_selection$`Pr(>F)`[1] >= .05 & levene_stop$`Pr(>F)`[1] >= .05 & shapiro$p.value >= .05)
}


assumption


# linear regression ----

model <- table_results %>%
  filter (area == area_) %>%
  lm(RMSE ~ selection, data = .)
summary(model)

# analysis ----

anova_results <- data.frame()

for(area_ in areas)
{

  correction <- ifelse(assumption[[area_]], 'none', 'bonferroni')

  anova <- table_results %>%
    filter (area == area_) %>%
    oneway.test(RMSE ~ selection, data = ., var.equal = assumption[[area_]])

  post_hoc <- table_results %>%
    filter (area == area_) %>%
    pairwise_t_test(formula = RMSE ~ selection, data = .,
                    comparisons = list(
                      c('Aleatorio', 'MIF'),
                      c('Aleatorio', 'PR-1'),
                      c('Aleatorio', 'PR0'),
                      c('Aleatorio', 'PR1'),
                      c('Aleatorio', 'PR2'),
                      c('MIF', 'PR-1'),
                      c('MIF', 'PR0'),
                      c('MIF', 'PR1'),
                      c('MIF', 'PR2'),
                      c('PR-1', 'PR0'),
                      c('PR-1', 'PR1'),
                      c('PR-1', 'PR2'),
                      c('PR0', 'PR1'),
                      c('PR0', 'PR2'),
                      c('PR1', 'PR2')
                    ),
                    p.adjust.method = 'bonferroni') %>%
    data.frame()

  rownames(post_hoc) <- paste0(post_hoc$group1, '-', post_hoc$group2)

  m <- table_results %>%
    filter (area == area_) %>%
    group_by(selection) %>%
    dplyr::summarise(mean(RMSE)) %>%
    data.frame()

  rownames(m) <- m$sample

  sd <- table_results %>%
    filter (area == area_) %>%
    group_by(selection) %>%
    dplyr::summarise(sd(RMSE)) %>%
    data.frame()

  rownames(sd) <- sd$sample

  set.seed(1000)
  post.effect <- table_results %>%
    filter (area == area_) %>%
    rstatix::cohens_d(formula = RMSE ~ selection, data = .,
                      ci = FALSE) %>%
                      # ci= TRUE, ci.type = "bca", nboot = 1000) %>%
    data.frame()

  rownames(post.effect) <- paste0(post.effect$group1, '-', post.effect$group2)

  anova_results. <- data.frame(
    area = area_,
    f.anova = anova$statistic,
    p.anova = anova$p.value,
    p.unif.strat = post_hoc['stratified-uniform',8],
    p.rand.strat = post_hoc['stratified-random',8],
    p.rand.unif = post_hoc['uniform-random',8],
    d.strat.unif = post.effect['stratified-uniform',4],
    d.strat.unif.low = post.effect['stratified-uniform',7],
    d.strat.unif.high = post.effect['stratified-uniform',8],
    d.strat.rand = post.effect['stratified-random',4],
    d.strat.rand.low = post.effect['stratified-random',7],
    d.strat.rand.high = post.effect['stratified-random',8],
    d.unif.rand = post.effect['uniform-random',4],
    d.unif.rand.low = post.effect['uniform-random',7],
    d.unif.rand.high = post.effect['uniform-random',8],
    m.unif = m['uniform', 2],
    sd.unif = sd['uniform', 2],
    m.strat = m['stratified', 2],
    sd.strat = sd['stratified', 2],
    m.rand = m['random', 2],
    sd.rand = sd['random', 2]
  )

  anova_results <- rbind(anova_results, anova_results.)
}

anova_results
