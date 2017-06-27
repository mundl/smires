library(smires)
library(lubridate)
library(ggplot2)

is.intermittent(x = ampneyBrook, ndays = 5, consecutive = TRUE, threshold = 0.001)
is.intermittent(ampneyBrook)

balder <- check_ts(balder)
metric(balder, plot = T, fun_major = mean, fun_total = sum)


ggplot(filter(balder, year(time) == 1976) , aes(time, discharge)) +
  geom_line() +
  scale_y_log10() +
  geom_hline(yintercept = 0.001, col = 2) +
  theme_bw()

smires(balder, plot = T)

# threshold variation
a <- smires(ampneyBrook, plot = T)
a <- smires(ampneyBrook, threshold = 0.01, plot = T)
a <- smires(ampneyBrook, threshold = 0.1, plot = T)


# mean max duration
smires(balder, fun_major = max, fun_total = mean, drop_na = "major", plot = T)


# mean max duration, detailed
grouped <- group_by_interval(balder, minor_interval = intervals$month)
events <- find_events(grouped, rule = "start")

events %>%
  group_by(major, state) %>%
  drop_na_periods("major") %>%
  summarise(var = max(duration)) %>%
  group_by(state) %>%
  summarise(meanMaxDur = mean(var))


# all arguments
smires(balder, major = 1, minor = intervals$month, drop_na = "group",
       rule = "start", threshold = 0.001,
       fun_group = NULL, fun_minor = NULL, fun_major = NULL, fun_total = NULL,
       state = c("no-flow", "flow"), invar = "duration", outvar = "variable",
       drop = FALSE, plot = FALSE)

# changing the major interval: start of hydrological year
b <- smires(balder, major = 32, plot = T)

# changing the minor interval: seasonal analysis
smires(balder, major = 60, minor = intervals$fourSeasons,
       fun_minor = mean, plot = T)

smires(balder, major = 1, minor = intervals$week,
       fun_minor = mean, plot = T)

# demo split events
find_events(grouped) %>%
  plot_groups()

find_events(grouped, rule = "start") %>%
  plot_groups()

find_events(grouped, rule = "end") %>%
  plot_groups()



smires(balder, fun_major = max, fun_total = median, drop_na = "major", major = 32)

b <- smires(balder, fun_major = max, drop_na = "major", major = 32)
ggplot(b, aes(y = as.numeric(variable), x = state)) +
  geom_boxplot() + coord_flip() +
  labs(y = "Maximum duration in days") +
  theme_bw()

# time steps

# weekly ----
weekly <- balder[c(T, rep(F, 6)), ]
weekly <- check_ts(weekly)

yearStart <- 92
minor <- intervals$week

grouped <- group_by_interval(weekly, major_interval = yearStart, minor_interval = minor)
events <- find_events(grouped, rule = "start")

# plot_groups(events)


# monthly ----
monthly <- balder[mday(balder$time) == 1, ]
monthly <- check_ts(monthly)

yearStart <- 182
minor <- intervals$month

grouped <- group_by_interval(monthly, major_interval = yearStart, minor_interval = minor)
events <- find_events(grouped, rule = "start")


plot_groups(events)

