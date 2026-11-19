# Calculate threshold-based performance measures for logistic models
#
# This function evaluates the predictive performance of one or more binary
# logistic regression models over a range of threshold probabilities.
# It calculates standard threshold-based performance metrics using
# modEvA::optiThresh and modEvA::threshMeasures, and derives additional
# metrics such as false positive/negative rates and critical threshold values.
#
# For each model, the function returns:
#   - allMeasures: a data frame of performance metrics across thresholds
#                  (0.01 to 1.00), including confusion matrix components
#                  and derived statistics like false positive rate (FPR),
#                  false negative rate (FNR), and their difference.
#   - optEach:    a data frame of optimal thresholds, including both
#                  standard and custom criteria (e.g., crossover points,
#                  or sensitivity/commission targets).
#
# Args:
#   models: A named list of fitted glm model objects
#           (e.g., from MyBLMs()$models).
#   df:     The data frame used to fit the models.
#           Required to calculate classification errors.
#   plot:   Logical; whether to generate plots for thresholds
#           (e.g., from optiThresh and optiPair).
#           Default is FALSE.
#
# Returns:
#   A named list where each element corresponds to a model and contains:
#     - Model:      The model name (string).
#     - allMeasures: Data frame of threshold-based performance metrics
#                    for each probability.
#     - optEach:     Data frame summarising selected optimal thresholds
#                    and decision criteria.
#
# Example:
#   mods <- MyBLMs(df = dat, x1 = "MnPO4P",
#                  x2 = "MnNO3N_Wnt",
#                  y = "BinClassMac.num")
#   results <- getAllMeasures(models = mods$models, df = dat)
#   head(results$modBV$allMeasures)   # Performance metrics for one model
#   results$modBV$optEach             # Optimal thresholds for that model
#


getAllMeasures <- function(models, df, plot = FALSE) {

  measures_list <- lapply(names(models), function(mod_name) {
    mod <- models[[mod_name]]
    
    # Get all threshold-based measures from optiThresh
    OThresh <- optiThresh(model = mod, pch = 20, plot = plot)
    allMeasures <- OThresh$all.thresholds %>%
      mutate(
        p = as.numeric(rownames(OThresh$all.thresholds)),
        Model = mod_name
      ) %>%
      dplyr::select(Model, p, everything())
    
    # Calculate classification errors from confusion matrix for p = 0.01 to 1.00
    Errors <- data.frame()
    N <- nrow(df)
    for (n in 1:100) {
      p <- n / 100
      CMatrix <- threshMeasures(mod, thresh = p, plot = FALSE, verbosity = 0)$ConfusionMatrix
      
      Tp <- CMatrix[1,1]
      Fp <- CMatrix[1,2]
      Fn <- CMatrix[2,1]
      Tn <- CMatrix[2,2]
      
      # Errors <- rbind(Errors, data.frame(Probability = p, Tp, Fp, Fn, Tn))
      Errors <- rbind(Errors, data.frame(p, Tp, Fp, Fn, Tn))
    }
    
    # Join classification errors with allMeasures by Probability
    allMeasures <- left_join(allMeasures, Errors, by = "p")
    
    # Add FPR, FNR, and absolute difference
    allMeasures <- allMeasures %>%
      mutate(
        FPR = Fp / N,
        FNR = Fn / N,
        FPR_FNRdiff = abs(FPR - FNR)
      )
    # Extract optimal thresholds from optiThresh output    
    optEach <- OThresh$optimals.each
    
    # Define criteria values
    critVal1 <- 0.1
    critVal2 <- 0.9
    critVal3 <- 0.2
    
    # Helper function to safely extract threshold given a criterion and measure column
    getCritThresh <- function(df, measure_col, crit_val) {
      df[which.min(abs(df[[measure_col]] - crit_val)), "p"]
    }
    
    p_critCom <- getCritThresh(allMeasures, "Commission", critVal1)
    p_critOmi <- getCritThresh(allMeasures, "Omission", critVal1)
    p_critSens <- getCritThresh(allMeasures, "Sensitivity", critVal2)
    p_critSpec <- getCritThresh(allMeasures, "Specificity", critVal2)
    p_critCom3 <- getCritThresh(allMeasures, "Commission", critVal3)
    
    # Threshold at crossover of Omission/Commission
    OmisComis <- optiPair(model = mod, measures = c("Omission", "Commission"), plot = plot)
    p_OmisComis <- OmisComis$ThreshDiff
    valOmi <- OmisComis$measures.values[OmisComis$measures.values$Threshold == p_OmisComis, 1]
    
    
    # Threshold at crossover of OPR/UPR
    OPRUPR <- optiPair(model = mod, measures = c("OPR", "UPR"), plot = plot)
    p_OPRUPR <- OPRUPR$ThreshDiff
    valOPR <- OPRUPR$measures.values[OPRUPR$measures.values$Threshold == p_OPRUPR, 1]
    
    # Threshold at crossover of FPR/FNR (toolkit mismatch)
    mit <- which.min(abs(allMeasures$FNR - allMeasures$FPR))
    p_FPR_FNR <- allMeasures[mit, ]$p
    valFPR <- allMeasures[mit, ]$FNR
 
    # Threshold Min Commission with Omission <2x Commission
    p_min_com <- allMeasures %>%
      filter(!is.na(Commission), !is.na(Omission), !is.na(p)) %>%
      filter(Omission < 2 * Commission) %>%
      slice_min(Commission, with_ties = TRUE) %>%   # keep all ties
      summarise(p_avg = mean(p, na.rm = TRUE)) %>%
      pull(p_avg)%>%
      round(2)
    
    # Threshold Min Omission with Commission <2x Omission
    p_min_om <- allMeasures %>%
      filter(!is.na(Commission), !is.na(Omission), !is.na(p)) %>%
      filter(Commission < 2 * Omission) %>%
      slice_min(Omission, with_ties = TRUE) %>%
      summarise(p_avg = mean(p, na.rm = TRUE)) %>%
      pull(p_avg)%>%
      round(2)
    
    
       
    # Add rows to optEach
    optEach <- rbind(optEach,
                     data.frame(measure = "OmisComm", threshold = p_OmisComis, value = valOmi, type = "Cross-over Omis/Commis"),
                     data.frame(measure = "OPR_UPR", threshold = p_OPRUPR, value = valOPR, type = "Cross-over OPR/UPR"),
                     data.frame(measure = "TKitMM", threshold = p_FPR_FNR, value = valFPR, type = "Cross-over FPR/NPR"),
                     data.frame(measure = "Com0.1", threshold = p_critCom, value = critVal1, type = "equal 0.1"),
                     data.frame(measure = "Omi0.1", threshold = p_critOmi, value = critVal1, type = "equal 0.1"),
                     data.frame(measure = "Sens0.9", threshold = p_critSens, value = critVal2, type = "equal 0.9"),
                     data.frame(measure = "Spec0.9", threshold = p_critSpec, value = critVal2, type = "equal 0.9"),
                     data.frame(measure = "default", threshold = 0.5, value = NA, type = NA),
                     data.frame(measure = "MinCom", threshold = p_min_com, value = NA, type = NA),
                     data.frame(measure = "MinOm", threshold = p_min_om, value = NA, type = NA),
                     data.frame(measure = "Com0.2", threshold = p_critCom3, value = critVal3, type = "equal 0.2"))
    
    list(
      Model = mod_name,
      allMeasures = allMeasures,
      optEach = optEach
    )
  })
  
  # Combine all models results in named list
  names(measures_list) <- names(models)
  return(measures_list)
}  

#--------------------------------------------------------------------------------------

# addOptEachFields()
#
# Purpose:
# Adds classification performance measures to the optimal threshold
# summary produced by getAllMeasures().
#
# For each model, the function matches the selected probability
# thresholds to the corresponding values in allMeasures and adds:
#   - Cohen's kappa
#   - Commission
#   - Omission
#
# Arguments:
#   results  Named list returned by getAllMeasures()
#
# Returns:
#   Updated results list containing the additional fields in optEach.
#
# Dependencies:
#   Base R only.

addOptEachFields <- function(results) {
  updated_results <- lapply(results, function(mod) {
    allMeasures <- mod$allMeasures
    optEach <- mod$optEach
    
    idx <- match(optEach$threshold, allMeasures$p)
    optEach$kappa      <- allMeasures$kappa[idx]
    optEach$commission <- allMeasures$Commission[idx]
    optEach$omission   <- allMeasures$Omission[idx]
    
    mod$optEach <- optEach
    return(mod)
  })
  
  return(updated_results)
}

