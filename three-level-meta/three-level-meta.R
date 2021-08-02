## -----------------------------------------------------------------------------
## Script: Three-level Meta-analysis Illustration
##
## Author: Filippo Gambarota
##
## Date Created: 2021-08-02
## -----------------------------------------------------------------------------

# The idea is inspired by a thread on the meta-analysis mailing list
# https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2021-August/003021.html

# Packages ----------------------------------------------------------------

library(tidyverse)
library(latex2exp)
library(R.utils)

withSeed(expr = {
  x_points_1 <- tibble(
    x = rnorm(25, 0, 1),
    y = 0
  )
}, seed = 1000)

ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 0.3989423), alpha = 0.2) +
  stat_function(data = data.frame(x = c(-3, 3)), aes(x),
                fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + 
  geom_point(data = x_points_1,
             aes(x = x, y = y),size = 5, alpha = 0.3) +
  geom_point(aes(x = 1, y = 0), size = 7, col = "red") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.y = element_blank()) +
  xlab(latex2exp::TeX("$\\mu_{ij}$"))


R.utils::withSeed(expr = {
  x_points_2 <- tibble(
    x = rnorm(25, 0, 1),
    y = 0
  )
}, seed = 1000)

ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 0.3989423), alpha = 0.2) +
  stat_function(data = data.frame(x = c(-3, 3)), aes(x),
                fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + 
  geom_point(data = x_points_2,
             aes(x = x, y = y),size = 5, alpha = 0.3) +
  geom_point(aes(x = -1, y = 0), size = 7, col = "red") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.y = element_blank()) +
  xlab(latex2exp::TeX("$\\mu_{j}$")) +
  annotate("text", x = -1, y = 0.03, label = TeX("$\\mu_{ij}$",output='character'), parse=TRUE)


R.utils::withSeed(expr = {
  x_points_3 <- tibble(
    x = rnorm(25, 0, 1),
    y = 0
  )
}, seed = 1000)

ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 0.3989423), alpha = 0.2) +
  stat_function(data = data.frame(x = c(-3, 3)), aes(x),
                fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + 
  geom_point(data = x_points_3,
             aes(x = x, y = y),size = 5, alpha = 0.3) +
  geom_point(aes(x = 0.5, y = 0), size = 7, col = "red") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.y = element_blank()) +
  xlab(latex2exp::TeX("$\\mu$")) +
  annotate("text", x = 0.5, y = 0.03, label = TeX("$\\mu_{j}$",output='character'), parse=TRUE)