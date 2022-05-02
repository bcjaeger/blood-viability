#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model
model_viz <- function(model) {

  plots <- tibble(name = model$shap_vi$name[1:10])

  plots$fig <- map(plots$name, .f = function(ftr){

    gg_data <- tibble(observed = model$x[, ftr, drop=TRUE],
                      shap = model$shap_values[[ftr]])

    ggplot(gg_data) +
      aes(x = observed, y = shap) +
      geom_point() +
      labs(x = ftr)

  })

  out <- table.glue::as_inline(plots, 'name', 'fig')





}
