# Simple Poisson regression example ----
rm(list = ls())
library(goodGLM)

set.seed(2281843)

n <- 100L
b0 <- 1.0
b1 <- 0.1

x <- runif(n, 0, 1)
mu <- exp(b0 + b1*x)
y <- rpois(n, lambda = mu)

mod <- glm(y ~ x, family = poisson(link = 'log'))

gof_output <- goodGLM(mod, groups = 10L, group_mode = "variance")
gof_output



# Type 1 error rate example (Poisson regression) ----
rm(list = ls())
library(goodGLM)

set.seed(5192404)

n <- 100L
b0 <- 1.0
b1 <- 0.1
n_sim <- 10^4

df_vec <- numeric(n_sim)
stat_vec <- numeric(n_sim)

for (i in 1:n_sim) {
  x <- runif(n, 0, 1)
  mu <- exp(b0 + b1*x)
  y <- rpois(n, lambda = mu)
  mod <- glm(y ~ x, family = poisson(link = 'log'))

  gof_output <- goodGLM(mod, groups = 10L, group_mode = "variance")
  df_vec[i] <- gof_output$df
  stat_vec[i] <- gof_output$statistic
}

mean(stat_vec > qchisq(0.95, df = df_vec)) # 0.0507



# Type 1 error rate example (logistic regression) ----
rm(list = ls())
library(goodGLM)

set.seed(8470641)

n <- 100L
b0 <- 0.1
b1 <- 0.5
n_sim <- 10^4

df_vec <- numeric(n_sim)
stat_vec <- numeric(n_sim)

for (i in 1:n_sim) {
  x <- runif(n, 0, 1)
  mu <- 1/(1 + exp(-(b0 + b1*x)))
  y <- rbinom(n = n, size = 1L, prob = mu)
  mod <- glm(y ~ x, family = binomial(link = 'logit'))

  gof_output <- goodGLM(mod, groups = 10L, group_mode = "variance")
  df_vec[i] <- gof_output$df
  stat_vec[i] <- gof_output$statistic
}

mean(stat_vec > qchisq(0.95, df = df_vec)) # 0.0499

