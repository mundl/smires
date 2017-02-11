.split_multiyear <- function(x)
{
  if(nrow(x) > 1) stop("should just have one row... :-(")

  # end -1 because events could and exactly on Jan 1st
  start <- x$start[1]
  end <- x$end[1] - 1

  if(year(start) == year(end)) return(x)

  ep <- rev(seq(floor_date(end, unit = "year"), start, by = "-1 year") - 1)
  ep <- c(ep, end) + 1

  start <- c(start, head(ep, -1))
  end <- ep
  dur <- end - start

  merge(select(x, -start, -end, -duration),
        tibble(start = start, end = end, duration = dur))
}

.label_dry_events <- function(x)
{
  x <- filter(x, state == "dry" & duration >= 7)
  x <- summarize(x, x = (start + end)/2, y = year, label = event)

  return(x)
}

plot_events <- function(x, size = 5, label = FALSE)
{
  require(ggplot2)
  threshold <- attr(events, "threshold")

  x <- x %>% group_by(event, period, pid) %>% do(.split_multiyear(.))

  .day <- function(x) as.numeric(format(x, "%j"))

  # we need the complete sequence of years as factor levels
  # otherwise this years would be omitted in the plot
  year <- format(x$start, "%Y")
  yseq <- seq(min(as.numeric(year)), max(as.numeric(year)))
  x$year <- factor(year, levels = as.character(yseq))

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
    geom_hline(yintercept = seq(1, length(yseq), by = 2), col = "white", alpha = 0.2) +
    labs(title = paste0("Stream-Flow Permanence (threshold = ",
                        threshold, ")")) +
    scale_x_continuous(breaks = at, labels = month.abb, expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0.5), drop = F) +
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())

  if(label)
  {
    labels <- .label_dry_events(x)

    p <- p + geom_text(data = labels,
                       mapping = aes(x = x, y = y, label = label),
                       size = 2.5)
  }

  return(p)
}
