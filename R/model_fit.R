#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bv_data
#' @param subset
#' @param type
#' @param impute
#' @param one_hot
model_fit <- function(bv_data,
                      type_subset = "PAX",
                      type_fit = "xgb",
                      impute = FALSE,
                      one_hot = TRUE) {

  data_init <- bv_data |>
    filter(vial_type %in% type_subset)

  preproc <- bv_make_recipe(data_init,
                            one_hot = one_hot,
                            impute = impute)

  switch(type_fit,
         'xgb' = xgb_fit(data = data_init, preproc = preproc))



}
