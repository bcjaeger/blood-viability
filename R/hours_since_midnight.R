hrs_since_midnight <- function(military_time){

  parts <- military_time |>
    str_split(pattern = fixed(":"), simplify = TRUE) |>
    as.data.frame() |>
    set_names(c('hour', 'minute')) |>
    mutate(across(everything(), as.numeric))

  parts$hour + parts$minute/60

}
