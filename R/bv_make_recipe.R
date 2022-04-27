#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

bv_make_recipe <- function(bv_data,
                           one_hot = TRUE,
                           impute = FALSE) {

  out <- recipe(bv_data, outcome ~ .) |>
    step_select(
      -any_of(
        c("RIN",
          "ppt_type",
          "type",
          "labelid",
          "Viability",
          "low_rin",
          "low_viability",
          "participantGUID",
          "labelGUID",
          "MTP_SMPBID",
          "MTP_OGLABELID",
          "MTP_REPOLABELID",
          "pid",
          "bid",
          "recid",
          "fuaddcom_pcaa",
          "fuaddcom_pcab",
          "recordthread_pcab",
          "recordthread_pcaa",
          "vialLabel",
          "Live_cell_concentration")
      ),
      -contains('comments'),
      -starts_with("collForm"),
      -starts_with('notes'),
      -starts_with("d_")) |>
    step_nzv(all_predictors()) |>
    step_mutate(across(starts_with("t_"), hrs_since_midnight),
                across(starts_with("staffID"), factor)) |>
    step_unknown(all_nominal_predictors())

  if(impute)
    out <- out |>
      step_impute_median(all_numeric_predictors()) |>
      step_impute_mode(all_nominal_predictors())


  if(one_hot)
    out <- out |>
      step_dummy(all_nominal_predictors())

  out

}
