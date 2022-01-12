rm(list = ls())

set.seed(2281843)

n <- 100L
b0 <- 1.0
b1 <- 0.1

x <- runif(n, 0, 1)
mu <- exp(b0 + b1*x)
y <- rpois(n, lambda = mu)

mod <- glm(y ~ x, family = poisson(link = 'log'))

library(goodGLM)
gof_output <- goodGLM(mod, groups = 10L, group_mode = "variance")
gof_output
