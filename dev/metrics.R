library(smires)



find_events(balder, threshold = 0.05) %>%
  summarise_at(vars(duration), funs(max)) %>%
  group_by(state) %>%
  summarise_at(vars(-period, -state), funs(mean))


plot_events(find_events(ampneyBrook, period = "year", threshold = 0.01))

m <- find_events(ampneyBrook, period = "month", threshold = 0.05) %>%
  summarise_at(vars(duration), funs(max, mean))
plot_period(m, type = "ts")
