#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

bv_load_data <- function() {


  # This is vial-level data can be matched to
  #  - st_adulabels_ext
  #  - st_pedlabels_ext
  # 1:1 using labelid

  # data_bv has 1 row per labelid
  data_bv <- load_all('SAS_Blood_Viability') |>
    mutate(ppt_type = recode(ppt_type, 'ADULT' = 'ADU'))

  data_bv_split <- split(data_bv, data_bv$ppt_type)

  data_adu <- load_all('st_adulabels_ext')

  data_ped <- load_all('st_pedlabels_ext')

  data_bv_adu <- data_bv_split |>
    getElement('ADU') |>
    mutate(labelid = as.character(labelid)) |>
    left_join(data_adu, by = c('labelid' = 'labelID'))

  data_bv_ped <- data_bv_split |>
    getElement('PED') |>
    left_join(data_ped, by = c('labelid' = 'labelID'))

  list(adu = data_bv_adu,
       ped = data_bv_ped) |>
    map(
      ~ mutate(
        .x,
        outcome = case_when(
          type %in% c('PAX', 'PBMC QC') ~ low_rin,
          type %in% c('PBMC SHIPPED') ~ low_viability
        ),
        .before = 1
      )
    )


}
