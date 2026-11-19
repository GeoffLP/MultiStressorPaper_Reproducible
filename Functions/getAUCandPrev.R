# Calculate AUC and prevalence for a list of logistic models
#
# This function applies the AUC() function from the modEvA package to a list of
# fitted binary logistic regression models (for example, as returned by MyBLMs()).
# It extracts the Area Under the Curve (AUC) and the observed prevalence for
# each model.
#
# Args:
#   models: A named list of fitted glm model objects
#           (e.g., from MyBLMs()$models).
#   plot:   Logical; whether to generate AUC plots for each model.
#           Default is FALSE.
#
# Returns:
#   A data frame with one row per model, and columns:
#     - Model:      The name of the model (taken from the list names).
#     - AUC:        The area under the ROC curve.
#     - Prevalence: The observed prevalence of positive cases in the data.
#
# Example:
#   mods <- MyBLMs(df = dat,
#                  x1 = "MnPO4P",
#                  x2 = "MnNO3N_Wnt",
#                  y = "BinClassMac.num")
#   getAUCandPrev(mods$models)
#
getAUCandPrev <- function(models, plot = FALSE) {
  results <- lapply(models, function(mod) {
    res <- AUC(model = mod, plot = plot)
    data.frame(
      Model = deparse(substitute(mod)),  # not reliable here; will fix below
      AUC = res$AUC,
      Prevalence = res$prevalence
    )
  })
  
  # Use list names to assign model names correctly
  results_df <- do.call(rbind, results)
  results_df$Model <- names(models)
  rownames(results_df) <- NULL
  
  return(results_df)
}

