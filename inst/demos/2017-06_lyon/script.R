library(smires)
is.intermittent(x = ampneyBrook, ndays = 5, consecutive = TRUE, threshold = 0.001)
is.intermittent(ampneyBrook)

plot_intermittency(ampneyBrook)
plot_intermittency(balder)

discharge <- validate(balder)


spells <- find_spells(discharge)
print(spells)

char_con(discharge, fun_major = max, fun_total = mean)


discharge %>%
  group_by_interval() %>%
  drop_na_periods(period = "group") %>%
  group_by(major) %>%
  summarize(var = max(discharge)) %>%
  ungroup() %>%
  summarize(variable = mean(var))
