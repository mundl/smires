library(smires)

e <- find_events(balder, threshold = 0.05)
plot_events(e)

p <- assign_period(e)
p <- assign_period(e, interval = "month")

plot_events(p)

p <- assign_period(e, interval = "month", start = 100)
p <- assign_period(e, interval = "month", start = as.Date("2015-04-28"))
p <- assign_period(e, interval = "month", include.year = FALSE)
p <- assign_period(e, interval = "month", span = T)

s <- split_events(p, at = "chunk")

s %>% group_by(year, state) %>%
  summarise_at(vars(duration), funs(max)) %>%
  group_by(state) %>%
  summarise_at(vars(duration), funs(mean))


plot_events(find_events(ampneyBrook, period = "year", threshold = 0.01))

m <- find_events(ampneyBrook, period = "month", threshold = 0.05) %>%
  summarise_at(vars(duration), funs(max, mean))
plot_period(m, type = "ts")
