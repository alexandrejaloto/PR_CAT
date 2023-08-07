# rm(list = ls())
# # conditions <- c(
# #   'ALETF45',
# #   'ALETF20',
# #   'ALEEP30',
# #   'ALEEP30RE015',
# #   'MIFTF45',
# #   'MIFTF20',
# #   'MIFEP30',
# #   'MIFEP30RE015',
# #   # 'PR-1TF45',
# #   # 'PR-1TF20',
# #   # 'PR-1EP30',
# #   # 'PR-1EP30RE015',
# #   # 'PR0TF45',
# #   # 'PR0TF20',
# #   # 'PR0EP30',
# #   # 'PR0EP30RE015',
# #   'PR1TF45',
# #   'PR1TF20',
# #   'PR1EP30',
# #   'PR1EP30RE015',
# #   'PR2TF45',
# #   'PR2TF20',
# #   'PR2EP30',
# #   'PR2EP30RE015'
# #   # 'PR3TF45',
# #   # 'PR3TF20',
# #   # 'PR3EP30',
# #   # 'PR3EP30RE015'
# # )
#
# selection <- c(
#   'Aleatorio',
#   'Aleatorio',
#   'Aleatorio',
#   'Aleatorio',
#   'MIF',
#   'MIF',
#   'MIF',
#   'MIF',
#   # 'PR-1',
#   # 'PR-1',
#   # 'PR-1',
#   # 'PR-1',
#   # 'PR0',
#   # 'PR0',
#   # 'PR0',
#   # 'PR0',
#   'PR1',
#   'PR1',
#   'PR1',
#   'PR1',
#   'PR2',
#   'PR2',
#   'PR2',
#   'PR2'
#   # 'PR3',
#   # 'PR3',
#   # 'PR3',
#   # 'PR3'
# )
#
# stop <- c(
#   'TF45',
#   'TF20',
#   'EP30',
#   'EP30RE015',
#   'TF45',
#   'TF20',
#   'EP30',
#   'EP30RE015',
#   # 'TF45',
#   # 'TF20',
#   # 'EP30',
#   # 'EP30RE015',
#   # 'TF45',
#   # 'TF20',
#   # 'EP30',
#   # 'EP30RE015',
#   'TF45',
#   'TF20',
#   'EP30',
#   'EP30RE015',
#   'TF45',
#   'TF20',
#   'EP30',
#   'EP30RE015',
#   'TF45',
#   'TF20',
#   'EP30',
#   'EP30RE015'
# )

name_selection <- c(
  'ALE',
  'MIF',
  'PR1',
  'PR2',
  'PR3'
)

name_stop <- c(
  'TF45',
  'TF20',
  'EP30',
  'EP30RE015'
)

selection <- c(
  'Aleatorio',
  'MIF',
  'PR1',
  'PR2',
  'PR3'
)


stop <- c(
  'TF45',
  'TF20',
  'EP30',
  'EP30RE015'
)

selection <- rep(selection, each = length(name_stop))
stop <- rep(stop, length(name_selection))
conditions <- paste0(rep(name_selection, each = length(name_stop)), name_stop)
selection
stop
conditions
