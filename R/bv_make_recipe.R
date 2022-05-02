#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

bv_make_recipe <- function(bv_data,
                           one_hot = TRUE,
                           impute = FALSE) {

  out <- recipe(bv_data, outcome ~ .) |>
    step_nzv(all_predictors()) |>
    step_mutate(across(any_of(c("staffID",
                                "barcodeID_pcaa",
                                "barcodeID_pcab",
                                "coll_staffid")), factor))

  if(impute)
    out <- out |>
      step_impute_median(all_numeric_predictors()) |>
      step_impute_mode(all_nominal_predictors())
  else
    out <- out |>
      step_unknown(all_nominal_predictors())

  if(one_hot)
    out <- out |>
      step_dummy(all_nominal_predictors())

  out

}
