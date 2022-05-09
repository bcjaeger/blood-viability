#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bv_data
viz_staff_shipment <- function(bv_data) {

  data_to_summarize <- bv_data |>
    add_count(staffID, name = 'n_staffID') |>
    add_count(MTP_SHIPNUM, name = 'n_MTH_SHIPNUM') |>
    filter(n_staffID > 3, n_MTH_SHIPNUM > 3)

  bystaff <- data_to_summarize |>
    group_by(staffID) |>
    summarise(p = mean(outcome),
              n = n())


  ggplot(gg_data) +
    aes(y = MTP_SHIPNUM, x = staffID) +
    geom_point(aes(size = p))

}
