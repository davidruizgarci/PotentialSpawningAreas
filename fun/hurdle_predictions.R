# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management

# ------------------------------------------------------------------------------
# Combine arithmetic positive means with occurrence inside joint replicates
# ------------------------------------------------------------------------------

# 1. Combine matching cell/date rows and matching joint replicate columns -----
# Reject unpaired components and incomplete draws instead of silently misaligning them.
combine_hurdle_replicates <- function(probability, positive_mean) {
  if (!is.matrix(probability) || !is.matrix(positive_mean) ||
    !identical(dim(probability), dim(positive_mean))) {
    stop("Prediction dimensions differ")
  }
  if (is.null(colnames(probability)) || !identical(colnames(probability), colnames(positive_mean))) {
    stop("Joint replicate identifiers are missing or do not match")
  }
  if (is.null(rownames(probability)) || !identical(rownames(probability), rownames(positive_mean))) {
    stop("Cell/date identifiers are missing or do not match")
  }
  if (any(probability < 0 | probability > 1, na.rm = TRUE) ||
    any(positive_mean < 0, na.rm = TRUE) || any(is.infinite(c(probability, positive_mean)))) {
    stop("Invalid occurrence probability or positive arithmetic mean")
  }
  # Missing predictions must not silently reduce the number of replicates.
  if (anyNA(probability) || anyNA(positive_mean)) stop("Incomplete joint predictions")
  probability * positive_mean
}


# 2. Summarise an already-combined joint prediction matrix --------------------
# Its intervals describe the mean model conditional on the chosen workflow.
summarise_joint_predictions <- function(joint) {
  if (!is.matrix(joint) || anyNA(joint) || any(!is.finite(joint))) stop("Invalid joint predictions")
  data.frame(
    mean = rowMeans(joint), median = apply(joint, 1, median),
    lower = apply(joint, 1, quantile, probs = .025), upper = apply(joint, 1, quantile, probs = .975)
  )
}
