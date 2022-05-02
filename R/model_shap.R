#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bv_xgb_fits_pax
model_shap <- function(model) {

  if(inherits(model$fit, 'xgb.Booster')){

    shap_values <- model$fit |>
      predict(newdata = model$x, predcontrib = TRUE) |>
      as_tibble()

    shap_vi <- shap_values |>
      select(-BIAS) |>
      apply(2, function(x) mean(abs(x))) |>
      enframe() |>
      filter(value > 0) |>
      arrange(desc(value))

    shap_values <- select(shap_values, any_of(shap_vi$name))

    model$shap_values <- shap_values
    model$shap_vi <- shap_vi

    return(model)

  }


}
