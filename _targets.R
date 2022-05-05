## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

# TODO:
#  1. consider demographics form, height/weight/medical history
#  2. consider acute tests form and timing of tests/collections
#  3. consider appending adults and peds data.
#    include an indicator of adu versus ped if you do.

# TODO in recipe:
#  1. focus d_visit_pcaa and d_visit_pcab, difference should always be 1 day

# create:
#     anchor: count number of days since jan 1 2017
#     if timepoint is 24 hour timepoint use B instead of A
#     if timepoint %in% c(24 hr post, 24 hr Rest 3), use B; ow use A
#          diff b/t anchor and d_mencyc_pcaa
#          diff b/t anchor and d_lastfluvac_pcaa

tar_plan(

  bv_data = bv_load_data(),

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
