library(smires)
is.intermittent(x = ampneyBrook, ndays = 5, consecutive = TRUE, threshold = 0.001)
is.intermittent(ampneyBrook)

plot_intermittency(ampneyBrook)
plot_intermittency(balder)

discharge <- check_ts(balder)


events <- find_events(discharge)
plot_events(events)
print(events)

metric(discharge, period = "year", agg1  = "max", agg2 = "mean")

metric(discharge, period = "year", agg1  = "max", agg2 = c("sd", "mean"))
metric(discharge, period = "year", agg1  = "sum", agg2 = "mean")


find_events(discharge) %>% assign_period(interval = "year") %>%
  split_events() %>%
  drop_na_periods(year) %>%  group_by(year, state) %>%
  summarise_at(vars(duration), funs(max)) %>% group_by(state) %>%
  summarise_at(vars(-year, -state), funs(mean))


