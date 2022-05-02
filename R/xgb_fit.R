#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param preproc
xgb_fit <- function(data = data_adu_init, preproc = preproc) {

  prepped <- prep(preproc)

  train <- juice(prepped)

  y <- pull(train, outcome)

  x <- train |>
    select(-outcome) |>
    as.matrix()

  params <- list(objective = 'binary:logistic',
                 max_depth = 2,
                 eta = 0.3,
                 eval_metric = 'auc')

  fit_cv <- xgb.cv(data = x,
                   label = y,
                   nfold = 10,
                   nrounds = 5000,
                   early_stopping_rounds = 50,
                   params = params)

  fit <- xgboost(
    data = x,
    label = y,
    nrounds = fit_cv$best_iteration,
    params = params
  )

  list(fit = fit, x = x, y = y)

}
