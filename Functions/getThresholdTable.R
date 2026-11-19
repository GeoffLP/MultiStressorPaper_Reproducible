# Create a threshold comparison table across models
#
# This function takes a list of model results (each containing an "optEach"
# data frame with columns: measure, threshold, kappa, commission, omission).
# For each model, it extracts these columns, renames them with the model name
# (e.g. threshold_modP, kappa_modP, etc.), and merges everything into one
# table by "measure".
#
# The result is a data frame showing, for each measure (e.g. CCR, Sensitivity),
# the thresholds and related metrics from each model. The table is sorted
# by measure.

# Arguments:
#   results_list  Named list of model results containing an optEach
#                 data frame for each model.
#
# Returns:
#   Data frame comparing selected probability thresholds and
#   associated classification measures across models.
#
# Dependencies:
#   dplyr package.
#
# Note:
#   The input is expected to contain the kappa, commission and
#   omission fields added by addOptEachFields().

getThresholdTable <- function(results_list) {
  thresh_list <- lapply(names(results_list), function(list_name) {
    model_entry <- results_list[[list_name]]
    
    # Prefer the model's internal "Model" field if available
    mod_name <- if (!is.null(model_entry$Model)) model_entry$Model else list_name
    
    # Extract relevant columns
    df <- model_entry$optEach[, c("measure", "threshold", "kappa", "commission", "omission")]
    
    # Rename all metric columns with model name
    colnames(df)[-1] <- paste(colnames(df)[-1], mod_name, sep = "_")
    
    return(df)
  })
  
  # Full join all model threshold tables by 'measure'
  threshold_table <- Reduce(function(x, y) dplyr::full_join(x, y, by = "measure"), thresh_list)
  
  # Sort by measure
  threshold_table <- dplyr::arrange(threshold_table, measure)
  
  return(threshold_table)
}
