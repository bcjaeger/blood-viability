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

  # done with function here ----

  vi <- xgb.importance(model = fit)[1:10, ]

  shap <- fit |>
    predict(newdata = x, predcontrib = TRUE) |>
    as_tibble() |>
    select(all_of(vi$Feature))

  ftr <- vi$Feature[2]

  gg_data <- tibble(observed = train[[ftr]],
                    shap = shap[[ftr]])

  ggplot(gg_data) +
    aes(x = observed, y = shap) +
    geom_point()

  ggplot(data = train, aes(x = Htcmavg_hwwt, y = outcome)) +
    geom_point(position = position_jitter(height = 1/10)) +
    geom_smooth()




}
