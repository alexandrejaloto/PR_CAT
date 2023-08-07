# # resps2 <- resps
# resps <- resps2
#
# resps = resps[[rep]]
# bank = items[,1:3]
# content.names = 1:30
# content.props = rep(1/30, 30)
# content.items = items$CO_HABILIDADE
# met.content = "MCCAT"
# item.language = items$TP_LINGUA

simCAT_LC <- function(resps, bank, start.theta = 0, sel.method = 'MFI',
                      cat.type = 'variable', acceleration = 1,
                      met.weight = 'mcclarty', threshold = .30, rmax = 1,
                      content.names = NULL, content.props = NULL,
                      content.items = NULL, met.content = 'MCCAT',
                      stop = list(se = .3, hypo = .015, hyper = Inf),
                      item.language)
{

  # preparation ----

  # bank with all information (parameters, language and content)
  bank <- data.frame(
    bank,
    item.language,
    content.items
  )
  bank$content.items
  rownames(bank) <- paste0("I", 1:nrow(bank))

  mod <- bank[,1:3]
  names(mod) <- c('a1', 'd', 'g')
  mod$d <- -mod$a1*mod$d

  mod <- mirtCAT::generate.mirt_object(mod, '3PL')

  if(cat.type == 'variable' & is.null(stop$max.items))
  {
    warning('The maximum number of items was set to be nrow(bank)')
    max.items <- nrow(bank)
  }

  if(!is.null(stop$max.items))
    max.items <- stop$max.items

  results <- list()

  # objects -----------------------------------------------

  # theta and se
  score <- data.frame(matrix(ncol = 2))
  # did the CAT converge?  (for whole application)
  convergence <- c()
  # theta history (for whole application)
  theta.history <- list()
  # se history (for whole application)
  se.history <- list()
  # previous responses
  prev.resps <- list()

  # progress bar ----
  bar <- txtProgressBar(min = 0, max = nrow(resps), char = "|", style = 3)

  # simulation ----

  for (person in 1:nrow(resps))
  {

    # person <- 1

    # exclude items with high exposure rate

    # if it is the first person
    if (person == 1)
    {
      number_items_available <- 1:nrow(bank)
      bank_available <- bank

    } else {

      exposure <- simCAT:::exposure.rate(prev.resps, rownames(bank))

      # select available items
      number_items_available <- which (exposure$Freq <= rmax)
      bank_available <- bank[number_items_available,]
    }

    # number_items_available is now from bank_available
    number_items_available <- 1:nrow(bank_available)

    # if English
    if(language[person] == 0)
    {
      available_language <- number_items_available[(is.na(bank_available$item.language) | bank_available$item.language != 1)]
      bank_available <- bank_available[available_language,]
    }
    # if Spanish
    if(language[person] == 1)
    {
      available_language <- number_items_available[(is.na(bank_available$item.language) | bank_available$item.language != 0)]
      bank_available <- bank_available[available_language,]
    }

    # simulation ----

    pattern <- rep(NA, nrow(bank))
    end <- list(stop = FALSE)
    administered <- NULL
    theta.cat <- theta.hist <- start.theta
    SE <- se.hist <- 1

    while(!end$stop)
    {

      # select item ----

      item_select <- select.item(
        bank = bank_available,
        theta = theta.cat,
        administered = administered,
        sel.method = sel.method,
        cat.type = cat.type,
        threshold = threshold,
        SE = SE,
        acceleration = acceleration,
        max.items = max.items,
        content.names = content.names,
        content.props = content.props,
        content.items = content.items[number_items_available],
        met.content = met.content
      )

      # update administered items
      administered <- c(administered, item_select$item)

      # pattern: select from resps only the available items, and from them, the administered ones (and the person)
      pattern[available_language][administered] <- resps[,available_language][person,administered]

      # estimate theta
      theta <- data.frame(
        mirt::fscores(
          object = mod,
          response.pattern = pattern,
          quadpts = 40,
          theta_lim = c(-4, 4)
        )
      )

      # update theta
      theta.cat <- theta$F1

      # delta theta
      delta.theta <- abs(theta.cat - theta.hist[length(theta.hist)])

      # update theta history
      theta.hist <- c(theta.hist, theta.cat)

      # update SE
      SE <- theta$SE

      # delta SE
      delta.se <- se.hist[length(se.hist)] - SE

      # update SE history
      se.hist <- c(se.hist, SE)

      # compute information for theta.cat
      info <- calc.info(bank = bank_available, theta = theta.cat)
      info[administered] <- 0
      info <- max(info)

      # stop the CAT? ----

      end <- stop.cat(
        rule = stop,
        current = list(
          se = SE,
          delta.theta = delta.theta,
          info = info,
          applied = length(administered),
          delta.se = delta.se
        )
      )

    }

    # store results
    score[person,] <- c(theta.cat, SE)
    convergence[person] <- end$convergence
    theta.history[[person]] <- theta.hist
    se.history[[person]] <- se.hist
    prev.resps[[person]] <- rownames(bank_available)[administered]

    # progress bar
    setTxtProgressBar(bar, person)

  }

  names (score) <- c('theta', 'SE')

  results <- list(
    score = score,
    convergence = convergence,
    theta.history = theta.history,
    se.history = se.history,
    prev.resps = prev.resps
  )
  return(results)
}

