# solve_predictor_given_p()
#
# Calculates the value of a selected predictor corresponding to a
# specified probability of Good ecological status, using coefficients
# from a fitted logistic regression model.
#
# Assumes an additive logistic model without interaction terms.
# Predictors transformed using log10 are returned on their original scale.
#
# Returns:
#   Numeric vector of predicted values for the target predictor.
#
# Dependencies:
#   Base R only.

# Solve for one predictor value that achieves a target probability in a logistic model
# model:      fitted glm(..., family = binomial) or similar with logit link
# p_target:   desired probability (0 < p_target < 1)
# target_var: the name of the predictor to solve for (character)
# others:     named list or data.frame of other predictors (scalars or vectors)
# log10_vars: character vector of predictor names that the model uses as log10(x)
#             (values in 'others' should be supplied on the *original* scale)
# eps:        small number to clamp p_target away from 0/1
solve_predictor_given_p <- function(model, p_target, target_var, others,
                                    log10_vars = character(), eps = 1e-9) {
  # Basic checks
  if (!is.numeric(p_target) || length(p_target) != 1) stop("p_target must be a single number.")
  p_target <- min(max(p_target, eps), 1 - eps)  # clamp away from 0/1
  
  coefs <- coef(model)
  if (!"(Intercept)" %in% names(coefs)) stop("Model must have an intercept.")
  if (!target_var %in% names(coefs)) stop("target_var not found in model coefficients.")
  
  # All predictors in the model (excluding intercept)
  model_preds <- setdiff(names(coefs), "(Intercept)")
  
  # Require values for all other predictors present in the model
  needed <- setdiff(model_preds, target_var)
  others <- as.list(as.data.frame(others))
  if (!all(needed %in% names(others))) {
    missing <- setdiff(needed, names(others))
    stop(sprintf("Missing values for predictors: %s", paste(missing, collapse = ", ")))
  }
  
  # Vectorization length (max length of supplied vectors)
  n <- max(vapply(others[needed], length, integer(1)))
  rep_len2 <- function(x, n) if (length(x) == n) x else rep_len(x, n)
  
  # Build contribution from all non-target predictors (apply log10 where needed)
  contrib <- numeric(n)
  for (nm in needed) {
    vals <- rep_len2(others[[nm]], n)
    if (nm %in% log10_vars) {
      if (any(vals <= 0, na.rm = TRUE)) stop(sprintf("Non-positive values for log10 var '%s'.", nm))
      vals <- log10(vals)
    }
    contrib <- contrib + coefs[[nm]] * vals
  }
  
  # Logit of target probability (standard for logistic regression)
  eta <- log(p_target / (1 - p_target))
  
  # Solve for the target predictor on the model's scale
  numer <- eta - coefs[["(Intercept)"]] - contrib
  beta_t <- coefs[[target_var]]
  if (isTRUE(all.equal(beta_t, 0))) stop("Coefficient of target_var is (near) zero; cannot solve.")
  
  # If target is log10-scaled in the model, back-transform to original units
  if (target_var %in% log10_vars) {
    log10_x <- numer / beta_t
    x <- 10^log10_x
  } else {
    x <- numer / beta_t
  }
  
  return(x)
}

