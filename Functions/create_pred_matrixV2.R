# create_pred_matrix()
#
# Purpose:
# Creates a matrix of predicted values for a target environmental
# variable corresponding to a specified probability of Good ecological
# status, using a fitted logistic regression model.
#
# Predictions are calculated across a grid of values for one or two
# other predictors, with optional fixed values for additional predictors.
#
# Supports predictors transformed using log10.
#
# Arguments:
#   model            Fitted logistic regression model
#   p_target         Target probability of Good ecological status
#   target_var       Predictor for which values are calculated
#   target_var_round Number of decimal places for predicted values
#   row_var          Predictor defining matrix rows
#   row_vals         Values of the row predictor (original scale)
#   row_round        Number of decimal places for row values
#   col_var          Optional predictor defining matrix columns
#   col_vals         Values of the column predictor (original scale)
#   log10_vars       Names of predictors transformed using log10
#   other_fixed      Fixed values for any additional predictors
#   prefix           Prefix for prediction column names
#
# Returns:
#   A list containing:
#     matrix        Matrix of predicted target values
#     results_wide  Tibble containing predictor values and predictions
#
# Dependencies:
#   solve_predictor_given_p() - required
#   format_results_wide()     - optional formatting helper
#   tibble and dplyr packages
#
# Note:
#   The function assumes that the fitted model and supplied predictor
#   values use consistent units and transformations.

create_pred_matrix <- function(model, p_target,
                               target_var, target_var_round = 2,
                               row_var, row_vals, row_round = 2,
                               col_var = NULL, col_vals = NULL,
                               log10_vars = character(),
                               other_fixed = list(),
                               prefix = "P_pred_") {
  # ---- validation ----
  stopifnot(is.character(target_var), length(target_var) == 1L)
  stopifnot(is.character(row_var),    length(row_var)    == 1L)
  if (!is.list(other_fixed)) other_fixed <- as.list(other_fixed)
  if (row_var %in% names(other_fixed)) stop("`row_var` appears in `other_fixed`.")
  if (!is.null(col_var)) {
    stopifnot(is.character(col_var), length(col_var) == 1L)
    if (col_var %in% names(other_fixed)) stop("`col_var` appears in `other_fixed`.")
    if (is.null(col_vals) || length(col_vals) == 0L) stop("Provide non-empty `col_vals` when `col_var` is set.")
  }
  
  if (length(row_vals) == 0L) stop("`row_vals` must be a non-empty vector.")
  
  # model-scale vs original-scale columns for the row variable (for display only)
  row_model <- if (row_var %in% log10_vars) log10(row_vals) else row_vals
  row_orig  <- row_vals
  
  # helper for 2-D: solve target across all row_vals for a single column value
  get_col_vec <- function(cv) {
    others <- c(
      other_fixed,
      stats::setNames(list(row_vals), row_var),
      if (!is.null(col_var)) stats::setNames(list(cv), col_var) else NULL
    )
    out <- solve_predictor_given_p(
      model = model,
      p_target = p_target,
      target_var = target_var,
      others = others,
      log10_vars = log10_vars
    )
    if (!is.numeric(out) || length(out) != length(row_vals)) {
      stop("Unexpected solver output; expected numeric vector of length(row_vals).")
    }
    out
  }
  
  # build prediction matrix
  if (!is.null(col_var)) {
    # 2-D: one column per col_val
    pred_mat <- sapply(col_vals, get_col_vec, simplify = "matrix")
    colnames(pred_mat) <- paste0(prefix, col_var, "=", col_vals)
  } else {
    # 1-D: single column across the row grid
    vec <- get_col_vec(cv = NULL)     # cv not used when col_var is NULL
    pred_mat <- matrix(vec, ncol = 1L)
    colnames(pred_mat) <- prefix
  }
  
  # wide tibble: row labels + prediction columns
  results_wide <- tibble::tibble(
    !!paste0(row_var, "_model") := row_model,
    !!paste0(row_var, "_orig")  := row_orig
  )
  results_wide <- dplyr::bind_cols(results_wide, tibble::as_tibble(pred_mat))
  
  # optional formatting pass (your helper), now supports NULL col_var
  if (exists("format_results_wide", mode = "function")) {
    results_wide <- format_results_wide(
      results_w = results_wide,
      row_var   = row_var,
      col_var   = col_var,     # can be NULL
      prefix    = prefix,
      row_model_name = paste0(row_var, "_vals"),
      row_orig_name  = paste0(row_var, "_orig"),
      round_pred     = target_var_round,
      round_row_orig = row_round
    )
  }
  
  list(
    matrix       = pred_mat,
    results_wide = results_wide
  )
}
