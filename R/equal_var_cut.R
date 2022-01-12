#' "Equal variance cut"
#' Group observations so that the variance in each group is approximately constant.
#' (This is an internal function.)
#'
#' @param pred_ord See 'goodGLM'.
#' @param groups Number of groups.
#' @param weights_ord See 'goodGLM'.
#'
#' @return
equal_var_cut <- function(pred_ord, groups, weights_ord) {
  pred_ordU <- unique(pred_ord) # Unique preds
  pred_ordC <- as.numeric(table(pred_ord)) # Counts for each pred
  weights_ordU <- weights_ord[!duplicated(pred_ord)] # Same length as pred_ordU

  if (length(pred_ordU) < groups) {
    stop("Please choose a smaller group number (not enough data points).")
  }

  cutpoints <- vector(mode="numeric", length = groups + 1)
  cutpoints[1] <- pred_ordU[1] # Minimum
  cutpoints[groups + 1] <- pred_ordU[length(pred_ordU)] # Maximum

  for (gg in 2:groups) {
    cutpoints[groups - gg + 2] <- spatstat::weighted.quantile(x = pred_ordU,
                                                              w = weights_ordU*pred_ordC,
                                                              probs = (groups - gg + 1)/(groups - gg + 2))
    rm_ind <- pred_ordU < cutpoints[groups - gg + 2]
    pred_ordU <- pred_ordU[rm_ind] # Remove covered elements
    pred_ordC <- pred_ordC[rm_ind] # Remove covered elements
    weights_ordU <- weights_ordU[rm_ind] # Remove covered elements
  }

  return(cutpoints)
}
