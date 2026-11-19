# Custom R functions

This directory contains the helper functions used in the Greek river multistressor analysis. They support the evaluation of fitted logistic regression models, selection and comparison of classification probability thresholds, and calculation and presentation of conditional phosphorus boundaries.

The functions are supplied to reproduce the existing analysis; their calculations have not been modified for the public repository.

## Files and dependencies

| File | Function(s) | Purpose | R package dependencies | Other custom functions / input dependencies |
|---|---|---|---|---|
| `GetAllMeasures.R` | `getAllMeasures()` | Calculate threshold-based classification measures and candidate optimal thresholds. | `modEvA`, `dplyr` | None |
| `GetAllMeasures.R` | `addOptEachFields()` | Add kappa, commission and omission to selected threshold results. | Base R | Expects output from `getAllMeasures()` |
| `getAUCandPrev.R` | `getAUCandPrev()` | Extract apparent AUC and observed prevalence for fitted models. | `modEvA` | None |
| `getThresholdTable.R` | `getThresholdTable()` | Combine selected thresholds and performance measures across models. | `dplyr` | Expects `optEach` fields populated by `addOptEachFields()` |
| `solve_predictor_given_p.R` | `solve_predictor_given_p()` | Solve an additive logistic regression equation for a predictor value at a specified probability. | Base R | None |
| `format_results_wide.R` | `format_results_wide()` | Rename and round columns in the wide prediction table. | `dplyr`, `rlang` | None |
| `create_pred_matrixV2.R` | `create_pred_matrix()` | Generate grids of conditional predictor boundary values. | `dplyr`, `tibble` | Calls `solve_predictor_given_p()` and, if loaded, `format_results_wide()` |

Note that the file `create_pred_matrixV2.R` defines the function `create_pred_matrix()`; the filename and function name are intentionally different in the current notebooks.

## Where the functions are used

The three notebooks checked were:

- `02_Predictor_Screening.Rmd` — does not source or call these helper files.
- `03_Model_Selection.Rmd` — does not source or call these helper files.
- `04_Model_Evaluation_Boundaries_Uncertainty_Table6_FigureS6.Rmd` — sources all six files in Section 1.2 and uses their functions in the evaluation and boundary calculations.

In Notebook 04, `getAllMeasures()`, `addOptEachFields()`, `getAUCandPrev()`, `getThresholdTable()`, `create_pred_matrix()` and `solve_predictor_given_p()` are called directly. `format_results_wide()` is called indirectly by `create_pred_matrix()` when available.

## Processing relationships

Threshold evaluation:

```text
getAllMeasures() -> addOptEachFields() -> getThresholdTable()
getAUCandPrev() [separate apparent AUC and prevalence summary]
```

Conditional boundary calculations:

```text
create_pred_matrix() -> solve_predictor_given_p()
                     -> format_results_wide() [if loaded]
```

Notebook 04 also calls `solve_predictor_given_p()` directly in later boundary calculations.

## Loading the functions

Notebook 04 sources all six scripts using paths relative to the R project root, via `here::here("Functions")`. Keep the exact filenames used by the notebook. The scripts are sourced before any of the helper functions are called.

For reproducibility, run the notebooks in a fresh R session with the required packages installed. In particular, `modEvA` and `dplyr` must be attached for the unqualified calls in `getAllMeasures.R` and `getAUCandPrev.R`; `rlang` and `tibble` are accessed by namespace-qualified calls in other helpers and must be installed, but do not need to be attached separately for those calls.

## Scope and assumptions

- The boundary-solving helpers are designed for additive binomial logistic regression models with a logit link, with no interaction terms.
- Values supplied for predictors identified in `log10_vars` are on the original scale; solved target values are returned on the original scale.
- `create_pred_matrix()` returns an unrounded numeric prediction matrix and a presentation table; optional formatting affects the table, not the matrix.
- `getAUCandPrev()` reports apparent model performance, not spatially cross-validated AUC.
- `getAllMeasures()` uses the supplied data-frame row count in its derived error proportions; the data frame should correspond to the observations used for the fitted models.

This README documents the supplied function files and their use in the three notebooks reviewed. It does not replace a full fresh-session execution test of the repository.
