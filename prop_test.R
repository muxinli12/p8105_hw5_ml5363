prop_test=function(city_name){
  city_sim_homicides=
    homicides_city|>
    drop_na(disposition)|>
    filter(city_state==city_name)|>
    mutate(disposition = factor(disposition,
                                levels = c("Closed by arrest", "Closed without arrest", "Open/No arrest")
    )) |>
    group_by(disposition)|>
    summarise(n=n())|>
    complete(disposition, fill = list(n = 0)) |>
    pivot_wider(
      names_from = disposition,
      values_from = n,
      values_fill = 0
    )|>
    mutate(total_homicides=`Closed by arrest` + `Closed without arrest` + `Open/No arrest`)|>
    mutate(unsolved_homicides=`Closed without arrest` + `Open/No arrest`)|>
    mutate(prop=unsolved_homicides/total_homicides)
  
  prop.test(x = pull(city_sim_homicides, unsolved_homicides),
            n = pull(city_sim_homicides, total_homicides))|>
    tidy()|>
    bind_cols(city_sim_homicides)
  
}