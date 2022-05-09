#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bv_data
tabulate_rin <- function(bv_data) {

  tbl_data <- bv_data |>
    filter(vial_type == 'PAX') |>
    group_by(bid) |>
    summarize(outcome = mean(outcome),
              Htcmavg_hwwt = Htcmavg_hwwt[1],
              wtkg_pcab = wtkg_pcab[1],
              d_diff_lastfluvac = d_diff_lastfluvac[1],
              d_diff_mencyc = d_diff_mencyc[1],
              sex_psca = sex_psca[1]) |>
    mutate(
      bmi = wtkg_pcab / (Htcmavg_hwwt/100)^2,
      across(
        .cols = c(Htcmavg_hwwt,
                  wtkg_pcab,
                  bmi,
                  d_diff_lastfluvac,
                  d_diff_mencyc),
        .fns = cut_percentile,
        n_group = 4
      )
    )


  tbl_row_vars <-
    c(
      "Htcmavg_hwwt",
      "wtkg_pcab",
      "d_diff_lastfluvac",
      "d_diff_mencyc",
      "bmi"
    )

  tbl_data_smry <- list()

  for(i in tbl_row_vars){

    tbl_data_smry[[i]] <- tbl_data |>
      group_by(.data[[i]], sex_psca) |>
      summarize(outcome = mean(outcome)) |>
      set_names(c('level', 'sex', 'outcome'))

  }

  bind_rows(tbl_data_smry, .id = 'variable') |>
    pivot_wider(names_from = sex, values_from = outcome) |>
    mutate(across(.cols = c(`1`,`2`), table.glue::table_value)) |>
    gt::gt(groupname_col = 'variable', rowname_col = 'level') |>
    gt::cols_label(`1` = "Women", `2` = "Men") |>
    gt::tab_header(
      title = 'Mean of the proportion of vials with low RIN by sex',
      subtitle = 'Data presented for participants with PAX vial type'
    ) |>
    gt::cols_align('center') |>
    gt::tab_source_note(
      'Variables are presented in subgroups based on quartiles'
    )





}


cut_percentile <- function(x, n_group) {

  # this will likely need to be coerced; input$n_group is a character
  n_group <- as.numeric(n_group)

  probs <- seq(0, 1, length.out = n_group + 1)

  breaks <- quantile(x, probs, na.rm = TRUE)

  labels <- vector(mode = 'character', length = n_group)

  for(i in seq(n_group)){

    if(i < n_group)
      labels[i] <- table_glue("{breaks[i]} to \u2264{breaks[i+1]}")

    if(i == n_group)
      labels[i] <- table_glue("\u2265{breaks[i]}")

  }

  out <- cut(x,
             breaks = breaks,
             include.lowest = TRUE,
             right = FALSE,
             labels = labels)


  out


}
