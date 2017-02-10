plot_events <- function(x, size = 5)
{
  require(ggplot2)
  e <- events
  .day <- function(x) as.numeric(format(x, "%j"))

  # we need the complete sequence of years as factor levels
  # otherwise this years would be omitted in the plot
  year <- format(e$start, "%Y")
  rng <- range(as.numeric(year))
  e$year <- factor(year, levels = as.character(seq(rng[1], rng[2])))

  # recode stard and end as day of the year (julian day)
  # slightly incorrect for leap years...
  e$start <- .day(e$start)
  e$end <- .day(e$end)

  # position labels months
  breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 367)
  at <- (head(breaks, -1) + tail(breaks, -1) - 2) / 2

  ggplot(e, aes(col = state)) +
    geom_vline(xintercept = breaks, col = "white") +
    geom_segment(aes(x = start, xend = end + 1, y = year, yend = year),
                 size = size) +
    scale_x_continuous(breaks = at, labels = month.abb, expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0.5), drop = F) +
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
}




