rm(list = ls())
library (ggplot2)

# fixed length ----

# test length
N <- 45

graphic <- data.frame()

# acceleration parameter
for (k in -1:2)
  # item
  for (t in 1:45)
  {
    if (t == 1)
      W <- 0 else {
        W <- (sum(((2:t)-1)^k))/(sum(((2:N)-1)^k))
      }
    graphic <- rbind(
      graphic,
      data.frame(
        k = k,
        t = t,
        W = W
      )
    )
  }

graphic$k <- as.factor(graphic$k)

png (
  filename = paste0 ('graphics/acceleration_parameter_fixed.png'),
  width = 3200,
  height = 3200,
  units = "px",
  pointsize = 12,
  bg = "white",
  res = 500,
  restoreConsole = TRUE
)

ggplot(graphic, aes(x = t, y = W, linetype = k)) +
  geom_line() +
  labs(title='', x="Item position", y = "W") +
  theme_bw()

dev.off()

# end

# original equation
M <- 45
graphic2 <- data.frame()
for(t in 0:44)
  graphic2 <- rbind(
    graphic2,
    data.frame(
      t = t,
      W = t/M
    )
  )

graphic2

ggplot(graphic2, aes(x = t, y = W)) +
  geom_line() +
  labs(title='', x="Item position", y = "W") +
  theme_bw()

# variable length ----

ratio <- seq(0, 1, .001)

graphic <- data.frame()
t <- 1
# acceleration parameter
for (k in c(.5, 1:3))
  # ratio
  for (r in ratio)
  {
    if (t == 1)
      W <- 0 else {
        W <- r^k
      }
    graphic <- rbind(
      graphic,
      data.frame(
        k = k,
        ratio = r,
        W = W
      )
    )
    t <- t + 1
  }

graphic$k <- as.factor(graphic$k)

png (
  filename = paste0 ('graphics/acceleration_parameter_variable.png'),
  width = 3200,
  height = 3200,
  units = "px",
  pointsize = 12,
  bg = "white",
  res = 500,
  restoreConsole = TRUE
)

ggplot(graphic, aes(x = ratio, y = W, linetype = k)) +
  geom_line() +
  labs(title='', x="Base (formula ratio)", y = "W") +
  theme_bw()

dev.off()
