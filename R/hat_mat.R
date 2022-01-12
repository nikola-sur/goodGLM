#' Calculate the hat matrix
#' Calculate the hat matrix for a GLM object.
#' (This is an internal function.)
#'
#' @param mod
#'
#' @return
hat_mat <- function(mod) {
  mu <- mod$fitted.values
  eta <- mod$linear.predictors

  # Note: Dispersion parameter not necessary, since it cancels out in the hat
  # matrix equation
  phi <- summary(mod)$dispersion

  # Get partial mu / partial eta
  muEtaVec <- family(mod)$mu.eta(eta)

  # Get variances
  varVec <- family(mod)$variance(mu) * phi

  Wsqrt <- diag(muEtaVec/sqrt(varVec))
  X <- model.matrix(mod)

  # Use of 'solve' with two parameters should speed up the calculation
  WsqrtX <- Wsqrt %*% X
  H <- WsqrtX %*% solve(t(WsqrtX) %*% WsqrtX, t(WsqrtX))

  return(H)
}
