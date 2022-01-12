#' "Inverse Covariance Matrix"
#' Calculate the matrix used in the quadratic form for the GOF test.
#' (This is an internal function.)
#'
#' @param mod GLM object.
#' @param G See 'goodGLM'.
#'
#' @return
inverse_cov <- function(mod, G) {
  # Get dispersion parameter and variance of observations
  mu_hat <- mod$fitted.values
  phi <- summary(mod)$dispersion
  vars <- family(mod)$variance(mu_hat) * phi

  V_sqrt <- diag(sqrt(vars))
  H <- hat_mat(mod = mod)
  n <- nrow(H)
  I <- diag(rep(1, n))
  GV_sqrt <- G %*% V_sqrt

  Omega_hat <- 1/n * GV_sqrt %*% (I - H) %*% t(GV_sqrt)
  Omega_hat_inv <- MASS::ginv(Omega_hat) # Moore-Penrose inverse (see Moore 1977)

  matrix_rank <- Matrix::rankMatrix(Omega_hat_inv)[[1]]

  return(list(
    matrix = Omega_hat_inv,
    rank   = matrix_rank))
}
