#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param preproc
xgb_fit <- function(data, preproc) {

  set.seed(329)

  folds <- map_dfr(
    .x = seq(1, 5),
    .f = ~ group_vfold_cv(data = data,
                          v = 10,
                          group = 'bid')
  )

  params <- list(objective = 'binary:logistic',
                 max_depth = 4,
                 gamma = 0.5,
                 num_parallel_tree = 2,
                 colsample_bynode = 1/2,
                 eta = 0.0001,
                 eval_metric = 'auc')

  ftr_candidates <- 3:12

  for(f in ftr_candidates){
    folds[[glue('xgb_fit_{f}_ftr')]] <- NA_real_
  }


  for(i in seq_along(folds$splits)){

    print(i)

    .preproc <- prep(preproc, training = training(folds$splits[[i]]))

    train <- juice(.preproc)
    test <- bake(.preproc, new_data = testing(folds$splits[[i]]))

    train_y <- pull(train, outcome)

    train_x <- train |>
      select(-outcome) |>
      as.matrix()

    test_y <- pull(test, outcome)

    test_x <- test |>
      select(-outcome) |>
      as.matrix()

    folds_nested <- group_vfold_cv(data = training(folds$splits[[i]]),
                                   v = 5,
                                   group = 'bid') |>
      pull(splits) |>
      map(complement)

    fit_cv_nest <- xgb.cv(data  = train_x,
                          label = train_y,
                          folds = folds_nested,
                          nrounds = 5000,
                          early_stopping_rounds = 50,
                          params = params,
                          verbose = FALSE)

    nrounds <- ceiling(fit_cv_nest$best_iteration * 0.9)

    message("nrounds: ", nrounds)

    fit_filter <- xgboost(
      data  = train_x,
      label = train_y,
      nrounds = nrounds,
      params = params,
      verbose = FALSE
    )

    ftrs_ranked <- shap.values(xgb_model = fit_filter,
                               X_train = train_x) |>
      getElement('mean_shap_score') |>
      enframe() |>
      arrange(desc(value)) |>
      pull(name)

    for(n_ftr in ftr_candidates){

      vars_selected <- ftrs_ranked[seq(n_ftr)]

      dtrain <- xgb.DMatrix(train_x[, vars_selected], label = train_y)
      dtest <- xgb.DMatrix(test_x[, vars_selected], label = test_y)

      fit <- xgb.train(
        nrounds = nrounds,
        data = dtrain,
        watchlist = list(train = dtrain, eval = dtest),
        params = params,
        verbose = FALSE
      )

      auc <- fit$evaluation_log$eval_auc[nrounds]

      print(auc)

      if(!is_empty(auc)){
        folds[i, glue('xgb_fit_{n_ftr}_ftr')] <- auc
      }



    }

  }

  cv_smry <- folds |>
    summarize(across(starts_with('xgb_fit'), mean)) |>
    pivot_longer(everything()) |>
    arrange(desc(value)) |>
    mutate(n_ftr = as.numeric(str_extract(name, '\\d+')))

  ggplot(cv_smry, aes(x=n_ftr, y=value)) +
    geom_point()

  n_ftr <- cv_smry |>
    slice(1) |>
    pull(n_ftr)

  prepped <- prep(preproc)

  train <- juice(prepped)

  y <- pull(train, outcome)

  x <- train |>
    select(-outcome) |>
    as.matrix()

  folds_nested <- group_vfold_cv(data = data,
                                 v = 10,
                                 group = 'bid') |>
    pull(splits) |>
    map(complement)

  fit_cv <- xgb.cv(data = x,
                   label = y,
                   folds = folds_nested,
                   nrounds = 5000,
                   early_stopping_rounds = 100,
                   params = params)

  # avoid overfitting
  nrounds <- fit_cv$best_iteration * 0.90

  fit_filter <- xgboost(
    data = x,
    label = y,
    nrounds = nrounds,
    params = params
  )

  vars_selected <- shap.values(xgb_model = fit_filter,
                               X_train = x) |>
    getElement('mean_shap_score') |>
    enframe() |>
    arrange(desc(value)) |>
    slice(seq(n_ftr)) |>
    pull(name)

  fit <- xgboost(
    data = x[, vars_selected],
    label = y,
    nrounds = nrounds, # avoid overfitting
    params = params
  )

  # consider chopping the function here and sending output:
  # list xgb_model = fit, X_train = x[, vars_selected], and shap_id_key

  # To return the SHAP values and ranked features by mean|SHAP|
  shap_values <- shap.values(xgb_model = fit,
                             X_train = x[, vars_selected])

  shap_values$mean_shap_score

  shap_id_key <- tibble(ID = seq(nrow(x)),
                        labelid = data$labelid,
                        pid = data$bid)

  # To prepare the long-format data:
  shap_long <- shap.prep(xgb_model = fit,
                         X_train = x[, vars_selected])

  # shap.plot.summary(shap_long, dilute = TRUE)

  partial_plots <- map(
    .x = vars_selected,
    .f = ~ shap.plot.dependence(data_long = shap_long,
                                x = .x,
                                y = .x) +
      ggtitle(glue("SHAP values of {.x}"))
  )

  plotly::ggplotly(partial_plots[[4]])

  shap_int <- shap.prep.interaction(xgb_mod = fit,
                                    X_train = x[, vars_selected])

  shap.plot.dependence(data_long = shap_long,
                       data_int = shap_int,
                       x= vars_selected[1],
                       y = vars_selected[2],
                       color_feature = vars_selected[2]) |>
    plotly::ggplotly()

  plot_data <- shap.prep.stack.data(shap_contrib = shap_values$shap_score,
                                    n_groups = 8)

  shap.plot.force_plot(plot_data, zoom_in = TRUE, zoom_in_location = 25) |>
    plotly::ggplotly()

  shap.plot.force_plot_bygroup(plot_data)

  plot_data |>
    left_join(shap_id_key, by = 'ID') |>
    left_join(select(data, outcome, labelid)) |>
    group_by(group) |>
    summarize(outcome = mean(outcome))

  plot_data |>
    left_join(shap_id_key, by = 'ID') |>
    select(group, labelid) |>
    left_join(select(data,
                     outcome,
                     all_of(fit$feature_names),
                     labelid)) |>
    select(group, outcome, all_of(fit$feature_names)) |>
    tbl_summary(by = group)





  # shap_data = list()
  # shap_importance <- c()
  #
  # shap <- predict(fit, newdata = x, predcontrib = TRUE)
  #
  # for(i in names(data)){
  #
  #   shap_col_index <- which(str_detect(colnames(shap),
  #                                      pattern = paste0("^", i)))
  #
  #   if(!is_empty(shap_col_index)){
  #
  #     if(length(shap_col_index) > 1){
  #
  #       shap_values <- apply(shap[, shap_col_index],
  #                            MARGIN = 1,
  #                            FUN = sum)
  #
  #     } else {
  #
  #       shap_values <- shap[, shap_col_index, drop=TRUE]
  #
  #     }
  #
  #     shap_importance[i] <- mean(abs(shap_values))
  #     shap_data[[i]] <- data[[i]]
  #     shap_data[[paste(i, 'contrib', sep='_')]] <- shap_values
  #
  #   }
  #
  # }
  #
  # shap_data <- as_tibble(shap_data)
  #
  # shap_importance <- enframe(shap_importance) |>
  #   arrange(desc(value))


  list(fit = fit,
       x = x,
       y = y,
       shap_importance = shap_importance,
       shap_data = shap_data)

}
