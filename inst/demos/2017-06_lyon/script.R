library(smires)
is.intermittent(x = ampneyBrook, ndays = 5, consecutive = TRUE, threshold = 0.001)
is.intermittent(ampneyBrook)

plot_intermittency(ampneyBrook)
plot_intermittency(balder)

discharge <- check_ts(balder)


spells <- find_spells(discharge)
plot_spells(spells)
print(spells)

metric(discharge, period = "year", agg1  = "max", agg2 = "mean")

metric(discharge, period = "year", agg1  = "max", agg2 = c("sd", "mean"))
metric(discharge, period = "year", agg1  = "sum", agg2 = "mean")


find_spells(discharge) %>% assign_period(interval = "year") %>%
  split_spells() %>%
  drop_na_periods(year) %>%  group_by(year, state) %>%
  summarise_at(vars(duration), funs(max)) %>% group_by(state) %>%
  summarise_at(vars(-year, -state), funs(mean))


