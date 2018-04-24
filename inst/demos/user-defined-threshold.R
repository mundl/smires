#' ---
#' title: IRES metrics with a user-defined threshold
#' output:
#'   pdf_document: default
#' ---

#' You have to use the most recent version of smires to use the functions
#' described here. Please just (re-)install it from github. In case you run into troubles,
#' do not hesitate to send me a screenshot or a mail of the error/output. These
#' problems are most of the time easy to solve. (eg. by updating existing
#' packages.)

#+ eval=FALSE
install_github("mundl/smires")

#+ eval=TRUE, message=FALSE
library(smires)

#' Intermittency plot for a higher threshold. Using 0.05 l/s to get a nicer plot.
#+ fig.height = 6
plot_intermittency(ampneyBrook, threshold = 0.05)


#' Compute onset for each year (first of no-flow day per year).
char_binary(x = ampneyBrook, threshold = 0.05,
            fun_major = min, drop_na = "major",
            spell.vars = vars(jday = julian_day(onset)), state = "no-flow",
            complete = FALSE)


#' ## Mean onset (first of no-flow day per year)
#' `mean_day()` can also be used instead of `circular_mean()`
#' By providing a `fun_total` argument, this function is used to summarize all
#' the values above.

x <- char_binary(x = ampneyBrook, threshold = 0.05,
                 fun_major = min, fun_total = circular_mean, drop_na = "major",
                 spell.vars = vars(jday = julian_day(onset)), state = "no-flow",
                 complete = FALSE)


print(x)

#' When printed inside a data_frame() the value is rounded.
print(x$jday)

#' The same output (but nicer formatted) can be achieved by a wrapper function `tau0()`.
tau0(ampneyBrook, threshold = 0.05)

#' ## Variability of onset
#' Compute the variability of onset (1 = no variance, 0 = max variance).
char_binary(x = ampneyBrook, threshold = 0.05,
            fun_major = min, fun_total = circular_r, drop_na = "major",
            spell.vars = vars(jday = julian_day(onset)), state = "no-flow",
            complete = FALSE)

#' Variability of onset, using the wrapper function.
tau0r(ampneyBrook, threshold = 0.05, format = F)


#' ## Standard deviation of onset
#' Probably easier to interpret because units are days.
char_binary(x = ampneyBrook, threshold = 0.05,
            fun_major = min, fun_total = circular_sd, drop_na = "major",
            spell.vars = vars(jday = julian_day(onset)), state = "no-flow",
            complete = FALSE)


#' ## Example from previous Mail
#' I wouldn't use this metric anymore...
j <- smires:::.append_flow_state(balder, threshold = 0.005) %>%
  filter(state == "no-flow") %>%
  group_by_interval(major_interval = 1) %>%
  group_by(major) %>%
  mutate(jday = julian_day(time))

j %>%
  summarize(variable = circular_r(jday))


#' # Multiple Stations
#' Doing the analysis for all stations which are currently in the
#' smires dataset (takes half a minute on my PC).

#+ warning=FALSE, message=FALSE
f <- list(meanOnset = tau0, varOnset = tau0r)
onset <- multiple_metrics(smires, .funs = f, format = FALSE,
                          threshold = 0.005)

#' This is just a quick plot  demonstrating ggplot...

#+ warning=FALSE, message=FALSE
library(ggplot2)
basemap(onset) +
  geom_point(aes(col = meanOnset, size = varOnset), alpha = 0.4) +
  scale_color_gradientn(colors = terrain.colors(4),
                        breaks = intervals$fourSeasons)




