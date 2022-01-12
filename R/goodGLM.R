#' Generalized Hosmer-Lemeshow Test for GLMs
#' Performs a global goodness-of-fit test on objects of class 'glm'. The test is
#' based on a generalization of the Hosmer-Lemeshow test. For more information,
#' see the two papers by Surjanovic, Lockhart, and Loughin (2020) and
#' Surjanovic and Loughin (2021).
#'
#' @param mod GLM object.
#' @param groups Number of groups. Defaults to 10 (as in the original Hosmer-Lemeshow test).
#' @param group_mode Group mode. One of "trials" or "variance", which groups observations
#' so that the number of trials or the variance of the observations is approximately equal
#' in each group. Defaults to "variance".
#' @param print_warnings Boolean. whether to display warnings.
#'
#' @return
#' #'
#' Some parts of this code are based on
#' http://www.chrisbilder.com/categorical/Chapter5/AllGOFTests.R by Tom Loughin
#' Modified by Nikola Surjanovic.
#' Previous comment from 'AllGOFTests.R':
#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 1-10-13                                                     #
# PURPOSE: Functions to compute Hosmer-Lemeshow, Osius-Rojek, and   #
#     Stukel goodness-of-fit tests                                  #
#                                                                   #
# NOTES:                                                            #
#####################################################################
# Single R file that contains all three goodness-of fit tests


# Adapted from program published by Ken Kleinman as Exmaple 8.8 on the SAS and R blog, sas-and-r.blogspot.ca
#  Assumes data are aggregated into Explanatory Variable Pattern form.
#' @export
goodGLM <- function(mod, groups = 10L, group_mode = "variance", print_warnings = TRUE) {
  # Input checks ---
  # Are we providing the right type of object
  stopifnot("glm" %in% class(mod))

  # Check for errors in data input
  if(any(colnames(mod$model) == "(weights)")) {
    stop("Please do not provide data in explanatory variable pattern (EVP) format.")
  }

  # Check that provided 'group_mode' is correct
  if (!(group_mode %in% c("trials", "variance")))
    stop("Invalid input for parameter 'group_mode'. Use 'trials' or 'variance'.")


  # Extract data ---
  y <- mod$y # Observed proportions, or T and F
  mu_hat <- mod$fitted.values
  sample_size <- length(mu_hat)

  y_ord <- y[order(mu_hat)]
  mu_hat_ord <- mu_hat[order(mu_hat)]

  # Get dispersion parameter and variance of observations
  phi <- stats::summary.glm(mod)$dispersion
  vars <- stats::family(mod)$variance(mu_hat) * phi
  vars_ord <- vars[order(mu_hat)]


  # Create cutpoints ---
  if (group_mode == "variance") {
    cutpoints <- equal_var_cut(pred_ord = mu_hat_ord, groups = groups, weights_ord = vars_ord)
  } else if (group_mode == "trials") {
    cutpoints <- stats::quantile(x = mu_hat_ord, probs = (0:groups)/groups)
  }
  interval <- cut(mu_hat_ord, cutpoints, include.lowest = TRUE)


  # Create contingency tables ---
  counts = stats::xtabs(formula = cbind(y_ord, mu_hat_ord) ~ interval)
  G <- matrix(NA, nrow = groups, ncol = sample_size)

  interval_unord <- cut(x = mu_hat, breaks = cutpoints, include.lowest = TRUE)
  raw_resids <- vector(mode = 'numeric', length = groups)

  for (gg in (1:groups)) {
    # Find the residuals that we are summing
    ind_temp <- which(interval_unord == levels(interval)[gg])
    a <- rep(0, sample_size)
    a[ind_temp] <- 1
    G[gg, ] <- a

    raw_resids[gg] <- 1/sqrt(sample_size) * (counts[gg] - counts[groups + gg])
  }


  # Calculate the GOF test statistic ---
  inv_cov_mat_obj <- inverse_cov(mod = mod, G = G)
  inv_cov_mat <- inv_cov_mat_obj$matrix
  df <- inv_cov_mat_obj$rank
  chisq <- t(raw_resids) %*% inv_cov_mat %*% raw_resids
  p_value <- 1 - stats::pchisq(chisq, df)


  # Create a table for output ---
  grouped_var <- G %*% vars
  pear <- (counts[1:groups] - counts[(groups + 1):(2 * groups)])/sqrt(grouped_var)
  counts_table <- cbind(counts, pear)
  colnames(counts_table)[3] <- 'pear'

  if (any(as.numeric(grouped_var) < 5) & print_warnings)
    warning("Some groups might not contain enough observations. Try using a smaller number of groups.")


  # Return object ---
  return(structure(list(
    method = paste0("Generalized Hosmer-Lemeshow (GHL) test with ", groups, " groups."),
    data_name = deparse(substitute(mod)), # Gives a string
    statistic = c(chisq = chisq),
    df = c(df = df),
    p_value = p_value,
    cutpoints = cutpoints,
    counts_table = counts_table,
    G = G,
    grouped_var = grouped_var,
    raw_resids = raw_resids
  ), class = 'htest')) # Hypothesis test class
}
