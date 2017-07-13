library(smires)
library(lubridate)
library(ggplot2)

# quickly check for intermittency
is.intermittent(x = ampneyBrook, ndays = 5,
                consecutive = TRUE,
                threshold = 0.001)
is.intermittent(ampneyBrook)

# check input time series
balder <- validate(balder)

# visualize time series and compute first metric
metric(balder, plot = T, fun_group = mean, major =
       fun_total = median)


# deriving a binary time series
ggplot(filter(balder, year(time) == 1976) , aes(time, discharge)) +
  geom_line() +
  scale_y_log10() +
  geom_hline(yintercept = 0.001, col = 2) +
  theme_bw()

smires(balder, plot = T)

# threshold variation
a <- smires(ampneyBrook, plot = T)
a <- smires(ampneyBrook, threshold = 0.1, plot = T)


# mean max duration
smires(balder, fun_major = max,
       fun_total = mean,
       drop_na = "major", plot = T)


# mean max duration, detailed
grouped <- group_by_interval(balder,  minor_interval = intervals$month)
spells <- find_spells(grouped, rule = "onset", threshold = 1)

spells %>%
  group_by(major, state) %>%
  drop_na_periods("major") %>%
  summarise(var = max(duration)) %>%
  group_by(state) %>%
  summarise(meanMaxDur = mean(var))


# all arguments
smires(balder, major = 1, minor = intervals$month, drop_na = "group",
       rule = "onset", threshold = 0.001,
       fun_group = NULL, fun_minor = NULL, fun_major = NULL, fun_total = NULL,
       state = c("no-flow", "flow"), invar = "duration", outvar = "variable",
       drop = FALSE, plot = FALSE)

# changing the major interval: start of hydrological year
b <- smires(balder, major = 32, plot = T)

# changing the minor interval: seasonal analysis
smires(balder, major = 60,
       minor = intervals$fourSeasons,
       fun_minor = mean, plot = T)

smires(balder, major = 1, minor = intervals$week,
       fun_minor = mean, plot = T)

# demo split spells
find_spells(grouped) %>%
  plot_groups()

find_spells(grouped, rule = "onset") %>%
  plot_groups()

find_spells(grouped, rule = "termination") %>%
  plot_groups()



# single value or distribution
smires(balder, fun_major = max, fun_total = median, drop_na = "major", major = 32)

b <- smires(balder, fun_major = max, drop_na = "major", major = 32)
ggplot(b, aes(y = as.numeric(variable), x = state)) +
  geom_boxplot() + coord_flip() +
  labs(y = "Maximum duration in days") +
  theme_bw()

# time steps

# weekly ----
weekly <- balder[c(T, rep(F, 6)), ]
weekly <- validate(weekly)

yearStart <- 92
minor <- intervals$week

grouped <- group_by_interval(weekly, major_interval = yearStart, minor_interval = minor)
spells <- find_spells(grouped, rule = "start")
spells
# plot_groups(spells)


# monthly ----
monthly <- balder[mday(balder$time) == 1, ]
monthly <- validate(monthly)

yearStart <- 182
minor <- intervals$month

grouped <- group_by_interval(monthly, major_interval = yearStart, minor_interval = minor)
spells <- find_spells(grouped, rule = "start")


plot_groups(spells)

