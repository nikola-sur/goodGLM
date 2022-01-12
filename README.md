 A generalized Hosmer-Lemeshow (GHL) goodness-of-fit test for a variety of generalized linear models (GLMs).
 
# Installation
To install this package, type:
 
 ``` r
 # install.packages('devtools')
 devtools::install_github('https://github.com/nikola-sur/goodGLM/')
 ```
 
# Example usage
First, we prepare some artificial data.
``` r
n <- 100L
x <- runif(n, 0, 1)
y <- rpois(n, .., ..)
```

Then, we fit a GLM and perform the goodness-of-fit test!
``` r
mod <- glm(y ~ x, family = poisson(link = 'log'))

library(goodGLM)
gof_output <- goodGLM(mod, groups = 10L, group_mode = ..)
gof_output
```

# Citation
If you use this package, consider citing one of our two papers on the GHL test:

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
