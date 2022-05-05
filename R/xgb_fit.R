#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param preproc
xgb_fit <- function(data, preproc) {

  prepped <- prep(preproc)

  train <- juice(prepped)

  ### informal work - checking 2-way interaction

  # gb_fit <- gbm(outcome ~ .,
  #     data = train,
  #     n.trees = 500,
  #     cv.folds = 5,
  #     shrinkage = 0.01,
  #     interaction.depth = 3)
  #
  # vip::vint(object = gb_fit,
  #           n.trees=205,
  #           feature_names = gb_fit$var.names) -> int
  #
  # int

  ### MTP_SHIPNUM*staffID only interaction that shows up

  y <- pull(train, outcome)

  x <- train |>
    select(-outcome) |>
    as.matrix()

  params <- list(objective = 'binary:logistic',
                 max_depth = 2,
                 colsample_bynode = 0.4,
                 eta = 0.1,
                 eval_metric = 'auc')

  fit_cv <- xgb.cv(data = x,
                   label = y,
                   nfold = min(ceiling(sum(y)/10), 10),
                   nrounds = 5000,
                   early_stopping_rounds = 50,
                   params = params)

  fit <- xgboost(
    data = x,
    label = y,
    nrounds = fit_cv$best_iteration * 0.90, # no overfitting please
    params = params
  )

  shap_data = list()
  shap_importance <- c()

  shap <- predict(fit, newdata = x, predcontrib = TRUE)

  for(i in names(data)){

    shap_col_index <- which(str_detect(colnames(shap),
                                       pattern = paste0("^", i)))

    if(!is_empty(shap_col_index)){

      if(length(shap_col_index) > 1){

        shap_values <- apply(shap[, shap_col_index],
                             MARGIN = 1,
                             FUN = sum)

      } else {

        shap_values <- shap[, shap_col_index, drop=TRUE]

      }

      shap_importance[i] <- mean(abs(shap_values))
      shap_data[[i]] <- data[[i]]
      shap_data[[paste(i, 'contrib', sep='_')]] <- shap_values

    }

  }

  shap_data <- as_tibble(shap_data)

  shap_importance <- enframe(shap_importance) |>
    arrange(desc(value))

  list(fit = fit,
       x = x,
       y = y,
       shap_importance = shap_importance,
       shap_data = shap_data)

}
