 A generalized Hosmer-Lemeshow (GHL) goodness-of-fit test for a family of generalized linear models (GLMs).
 
# Installation
To install this package, type:
 
 ``` r
 # install.packages('devtools')
 devtools::install_github('https://github.com/nikola-sur/goodGLM/')
 ```
 
 If you don't have 'devtools' installed, when asked if you would like to install from source, select "no". (This way the installation should be faster.)
 
# Example usage
First, we prepare some artificial data.
``` r
n <- 100L
b0 <- 1.0
b1 <- 0.1

x <- runif(n, 0, 1)
mu <- exp(b0 + b1*x)
y <- rpois(n, lambda = mu)
```

We then fit a GLM and perform the goodness-of-fit test!
``` r
mod <- glm(y ~ x, family = poisson(link = 'log'))

library(goodGLM)
gof_output <- goodGLM(mod, groups = 10L, group_mode = "variance")
gof_output
```

# Citation
If you use this package, please consider citing the package

```
@software{goodGLM_2022,
author = {Nikola, Surjanovic and Richard, Lockhart and Thomas, Loughin},
month = {2},
title = {{'goodGLM': A generalized Hosmer-Lemeshow goodness-of-fit test for a family of generalized linear models}},
url = {https://github.com/nikola-sur/goodGLM},
version = {0.0.0.9},
year = {2022}
}
```

and/or one of our two papers on the GHL test:

```
@article{surjanovic2020generalized,
  title={A Generalized Hosmer-Lemeshow Goodness-of-Fit Test for a Family of Generalized Linear Models},
  author={Surjanovic, Nikola and Lockhart, Richard and Loughin, Thomas M},
  journal={arXiv preprint arXiv:2007.11049},
  year={2020}
}

@article{surjanovic2021improving,
  title={Improving the Hosmer-Lemeshow Goodness-of-Fit Test in Large Models with Replicated Trials},
  author={Surjanovic, Nikola and Loughin, Thomas M},
  journal={arXiv preprint arXiv:2102.12698},
  year={2021}
}
```
