## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  bv_data = bv_load_data(),

  bv_xgb_fits_pax = model_fit(bv_data,
                              type_subset = 'PAX',
                              type_fit = 'xgb',
                              impute = FALSE,
                              one_hot = TRUE)



)
