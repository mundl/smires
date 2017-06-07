.label_noflow_events <- function(x)
{
  x <- filter(x, state == "no-flow" & duration >= 7)
  x <- summarize(group_by(x, start),
                 x = (start + end)/2, y = year, label = event)

  return(x)
}

plot_events <- function(x, size = 5, label = TRUE)
{
  requireNamespace("ggplot2")
  threshold <- attr(x, "threshold")

  # nothing to plot for 0day events
  #
  # make sure events are split by year
  if(is.null(attr(x, "intervall"))) x <- assign_period(x, interval = "year")

  x <- filter(x, duration > 0) %>%
    split_events(at = "year", rule = "cut")

  .day <- function(x) as.numeric(format(x, "%j"))

  # we need the complete sequence of years as factor levels
  # otherwise this years would be omitted in the plot
  year <- format(x$start, "%Y")
  x$year <- factor(year, levels = full_seq(as.numeric(year), 1))

  # recode stard and end as day of the year (julian day)
  # slightly incorrect for leap years...
  x$start <- .day(x$start)
  x$end <- .day(x$end)
  x$end[x$end == 1] <- 366

  # position labels months
  breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 367)
  at <- (head(breaks, -1) + tail(breaks, -1) - 2) / 2

  p <- ggplot(x) +
    # todo: use geom_rect
    geom_segment(aes(x = start, xend = end, y = year, yend = year,
                     col = state), size = size) +
    geom_vline(xintercept = breaks, col = "white", alpha = 0.5) +
    geom_hline(yintercept = seq(1, nlevels(x$year), by = 2), col = "white", alpha = 0.2) +
    labs(title = paste0("Stream-Flow Permanence (threshold = ",
                        threshold, ")")) +
    scale_x_continuous(breaks = at, labels = month.abb, expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0.5), drop = F) +
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())

  if(label)
  {
    labels <- .label_noflow_events(x)

    p <- p + geom_text(data = labels,
                       mapping = aes(x = x, y = y, label = label),
                       size = 2.5)
  }

  return(p)
}



plot_period <- function(x, type = c("ts", "distribution"))
  # todo: duration is hardcoded
{
  type <- match.arg(type)

  if(!"duration" %in% colnames(x)) stop("There is no column called 'duration'. Hardcoded... :(")
  if(type == "distribution")
  {
   p <- ggplot(x, aes(duration)) + geom_density(trim = TRUE) + facet_wrap(~ state) +
      scale_x_continuous() + geom_rug() +
      labs(x = "Maximum Duration per Period (days)", title = "Distribution of Events")
  }

  if(type == "ts")
  {
    x <-  ungroup(x) %>%
      complete(year, state)

    p <- ggplot(x, aes(year, duration, col = state, group = state)) +
      geom_point() + geom_line() +
      expand_limits(y = 0) +
      scale_y_continuous() +
      labs(y = "Maximum Duration (days)")
  }

  return(p)
}
