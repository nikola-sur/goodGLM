#' Calculate the hat matrix
#' Calculate the hat matrix for a GLM object.
#' (This is an internal function.)
#'
#' @param mod GLM object.
#'
#' @return
hat_mat <- function(mod) {
  mu <- mod$fitted.values
  eta <- mod$linear.predictors

  # The dispersion parameter is not necessary (it cancels out in the hat matrix equation)
  phi <- stats::summary.glm(mod)$dispersion

  mu_eta <- stats::family(mod)$mu.eta(eta) # Get partial mu / partial eta
  vars <- stats::family(mod)$variance(mu) * phi # Get variances
  Wsqrt <- diag(mu_eta/sqrt(vars))
  X <- stats::model.matrix(mod)

  WsqrtX <- Wsqrt %*% X
  H <- WsqrtX %*% solve(t(WsqrtX) %*% WsqrtX, t(WsqrtX)) # Get hat matrix

  return(H)
}
