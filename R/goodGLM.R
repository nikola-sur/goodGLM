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
#'
#' @return
#' @export
#'
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
goodGLM <- function(mod, groups = 10L, group_mode = "variance") {
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

  y_ord <- y[order(mu_hat)]
  mu_hat_ord <- mu_hat[order(mu_hat)]

  # Get dispersion parameter and variance of observations
  phi <- summary(mod)$dispersion
  vars <- family(mod)$variance(mu_hat) * phi
  vars_ord <- vars[order(mu_hat)]


  # Create cutpoints ---
  if (group_mode == "variance") {
    cutpoints <- equalVarCut(pred.o = mu_hat_ord, g=g, weights.o = vars_ord)
  } else if (group_mode == "trials") {
    cutpoints <- quantile(mu_hat_ord, 0:g/g)
  }
  interval <- cut(mu_hat_ord, cutpoints, include.lowest = TRUE)


  # Create contingency tables ---
  counts = xtabs(formula = cbind(y_ord, mu_hat_ord) ~ interval)

  sampleSize <- length(mu_hat)
  G <- matrix(NA, nrow=g, ncol=sampleSize)

  intervalUnO <- cut(mu_hat, cutpoints, include.lowest = TRUE)
  rawResids <- vector(mode='numeric',length=g)

  for (gg in (1:g)) {
    # Find the residuals that we are summing
    indTemp <- which(intervalUnO==levels(interval)[gg])
    a <- rep(0,sampleSize)
    a[indTemp] <- 1
    G[gg, ] <- a

    rawResids[gg] <- 1/sqrt(sampleSize) * (counts[gg] - counts[g+gg])
  }


  # Calculate the GOF test statistic ---
  invCovMatObject <- CalculateGroupedResidualInvCovMatHHGeneralized(mod, G)
  invCovMat <- invCovMatObject$matrix
  df <- invCovMatObject$rank
  chisq <- t(rawResids) %*% invCovMat %*% rawResids
  P <- 1 - pchisq(chisq, df)


  # Create a table for output ---
  groupedVar <- G %*% vars
  pear <- (counts[1:g] - counts[(g+1):(2*g)])/sqrt(groupedVar)
  countsTable <- cbind(counts, pear)
  colnames(countsTable)[3] <- 'pear'

  if (any(as.numeric(groupedVar) < 5) & warnings)
    warning("Some groups might contain too few observations. Try using a smaller number of groups.")


  # Return object ---
  return(structure(list(
    method = paste0("GHL test with ", g, " groups."),
    data.name = deparse(substitute(mod)), # Gives a string
    statistic = c(X2 = chisq),
    parameter = c(df = df),
    p.value = P,
    cutpoints = cutpoints,
    countsTable = countsTable,
    G = G,
    groupedVar = groupedVar,
    rawResids = rawResids
  ), class = 'htest')) # Hypothesis test class
}
