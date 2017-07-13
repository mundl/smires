library(smires)

e <- find_spells(ampneyBrook, threshold = 0.001)
plot_spells(e)

p <- assign_period(e, interval = "month")

plot_spells(p)

# p <- assign_period(e, interval = "month", start = 100)
# p <- assign_period(e, interval = "month", start = as.Date("2015-04-28"))
# p <- assign_period(e, interval = "month", include.year = FALSE)
# p <- assign_period(e, interval = "month", span = T)

s <- split_spells(assign_period(e), at = "year")

p <- s %>% drop_na_periods(year) %>% group_by(year, state) %>%
  summarise_at(vars(duration), funs(max))

p %>%
  group_by(state) %>%
  summarise_at(vars(duration), funs(mean))


plot_spells(find_spells(ampneyBrook, period = "year", threshold = 0.01))

m <- find_spells(ampneyBrook, period = "month", threshold = 0.05) %>%
  summarise_at(vars(duration), funs(max, mean))
plot_period(m, type = "ts")
