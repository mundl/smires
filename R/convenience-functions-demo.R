plot_intermittency <- function(...)
{
  require(ggplot2)
  plot_events(find_events(...), label = FALSE) +
    theme(legend.position = "right")
}


metric <- function(x, fun, period = "year", agg1, agg2) {
  find_events(x) %>% assign_period(interval = period) %>%
    split_events() %>%
    drop_na_periods(year) %>%  group_by(year, state) %>%
    summarise_at(vars(duration), funs_(agg1))  %>% group_by(state) %>%
    summarise_at(vars(-year, -state), funs_(agg2))
}

mean_annual_max_duration_dry <- function(x)
{
  y <- metric(x, period = "year", agg1  = "max", agg2 = "mean")
  y$duration[y$state == "no-flow"]
}

mean_annual_number_dry_days <- function(x)
{
  y <- metric(x, period = "year", agg1  = "sum", agg2 = "mean")
  y$duration[y$state == "no-flow"]
}