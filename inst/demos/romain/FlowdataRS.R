library(devtools)
install_github("mundl/smires")

library(tidyverse)
library(smires)

# import your data
filename <- "inst/demos/romain/FlowdataRS.csv"
infile <- filename %>%
  read_csv2(col_types = cols(
    `Time stamp` = col_date(format = "%d.%m.%Y")
  )) %>%
  rename(time = "Time stamp")

# reshape data into long format
station <- infile %>%
  gather(key = "station", value = "discharge", -time)

# nest data into a list column and append meta data to that list column
station <- station %>%
  group_by(station) %>%
  nest() %>%
  mutate(data = map(data, validate)) %>%
  deframe_smires()


# define starts of seasons
starts <- as.Date(c("2011-03-01", "2011-06-01", "2011-09-01", "2011-12-01"))
starts <- as.numeric(format(starts, "%j"))
names(starts) <- c("spring", "summer", "fall", "winter")

# this is equivalent to one of the prefined seasons
seasons <- intervals$fourSeasons

# some metrics for a single station -----
walkern <- station$data[[2]]

plot_intermittency(walkern, minor = intervals$fourSeasons)

# table of events, events are per default splitted at season boundaries
char_binary(walkern, minor = seasons)


# within each group (group = the combination of season and year)
# we want to sum up all durations: fun_group = sum
#
# for each minor interval (= season, in your case)
# we want the mean durations: fun_minor = mean

# drop the whole major interval (year) if it contains missing values
# drop_na = "major"

char_binary(walkern, minor = seasons,
            fun_group = sum, fun_minor = mean,
            drop_na = "major")

# the additional NA values of the minor inteval are a bug
# I will fix it some days... ;-)


# metrics for multiple stations ----
# now we can wrap this into a function
mean_seasonal_no_flow_days <- function(x)
{
  char_binary(x = x, minor = seasons,  fun_group = sum, fun_minor = mean,
              drop_na = "major", state = "no-flow")
}

# and define a second function for the standard deviation per season
sd_seasonal_no_flow_days <- function(x)
{
  char_binary(x = x, minor = seasons,  fun_group = sum, fun_minor = sd,
              drop_na = "major", state = "no-flow")
}


result <- station %>%
  mutate(`mean number of low flow days` = map(data, mean_seasonal_no_flow_days),
         `standard deviation` = map(data, sd_seasonal_no_flow_days)) %>%
  select(-data)

result <- result %>%
  gather(key = "metric", value = "data", -station) %>%
  mutate(data = map(data, function(x) {x$duration <- as.numeric(x$duration); x})) %>%
  unnest() %>%
  na.omit() %>%
  arrange(station, metric, minor)

ggplot(result, aes(x = minor, y = duration, col = station, group = station)) +
  geom_point() +
  geom_path() +
  facet_wrap(~metric, ncol = 1)


# Intermittency plot for multiple stations ----
pdf("intermittency.pdf", width = 21/2.54, height = 29.7/2.54)
walk(station$data, plot_intermittency, minor = intervals$fourSeasons)
dev.off()






