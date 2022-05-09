## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

# TODO:
#  1. consider demographics form, height/weight/medical history
#  2. consider acute tests form and timing of tests/collections
#  3. try age/sex/weight/diff fluvac model

tar_plan(

  bv_data = bv_load_data(),

  tbl_rin = tabulate_rin(bv_data),

  # adu data have 12 variables that represent muscle and
  # adipose in EQC view (EQC ?= extended quality control),
  # these variables should always be missing in peds data.

  bv_xgb_fits_pax = model_fit(bv_data,
                              type_subset = 'PAX',
                              type_fit = 'xgb',
                              impute = FALSE,
                              one_hot = TRUE),

  bv_xgb_fits_pbmc = model_fit(bv_data,
                               type_subset = 'PBMC SHIPPED',
                               type_fit = 'xgb',
                               impute = FALSE,
                               one_hot = TRUE)


)
