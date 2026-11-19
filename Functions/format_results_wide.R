# Generalise naming + rounding of the wide table
# - results_w: output$results_wide from create_pred_matrix()
# - row_var:   name of the row variable used in create_pred_matrix()
# - col_var:   name of the column variable used in create_pred_matrix()
# - prefix:    prefix used for prediction columns in create_pred_matrix() (default "P_pred_")
# - row_model_name / row_orig_name: pretty names you want in the final table
# - round_pred: digits for prediction columns
# - round_row_orig: digits for the original-scale row variable
format_results_wide <- function(results_w,
                                row_var,
                                col_var,
                                prefix ,
                                row_model_name = paste0(row_var, "_vals"),
                                row_orig_name  = paste0(row_var, "_orig"),
                                round_pred = 2,
                                round_row_orig = 2) {
  # we'll use rlang for tidy eval without attaching it
  requireNamespace("dplyr"); requireNamespace("rlang")
  
  # dynamic column names present in results_w
  row_model_col <- paste0(row_var, "_model")
  row_orig_col  <- paste0(row_var, "_orig")
  
  res <- results_w
  
  # rename row columns if they exist
  if (row_model_col %in% names(res)) {
    res <- dplyr::rename(res, !!rlang::sym(row_model_name) := !!rlang::sym(row_model_col))
  }
  if (row_orig_col %in% names(res)) {
    res <- dplyr::rename(res, !!rlang::sym(row_orig_name) := !!rlang::sym(row_orig_col))
  }
  
  # round prediction columns that match the chosen col_var & prefix
 # pred_prefix <- paste0(prefix, col_var)
  pred_prefix <- if (is.null(col_var)) prefix else paste0(prefix, col_var)
  res <- dplyr::mutate(
    res,
    dplyr::across(dplyr::starts_with(pred_prefix), ~ round(.x, round_pred))
  )
  
  # round the original-scale row variable (if present)
  if (row_orig_name %in% names(res)) {
    res <- dplyr::mutate(res, !!rlang::sym(row_orig_name) := round(!!rlang::sym(row_orig_name), round_row_orig))
  }
  
  res
}
