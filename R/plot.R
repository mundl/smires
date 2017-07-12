# .label_noflow_events <- function(x)
# {
#   ungroup(x) %>%
#     filter(state == "no-flow" & duration >= 7) %>%
#     mutate(x = (start + end)/2, y = year, label = event) %>%
#     select(label, x, y)
# }
#
# plot_events <- function(x, size = 5, label = TRUE)
# {
#   requireNamespace("ggplot2")
#   threshold <- get_attr_smires(x, key = "threshold")
#
#   # nothing to plot for 0day events
#   #
#   # make sure events are split by year
#   if(is.null(attr(x, "intervall"))) x <- assign_period(x, interval = "year")
#
#   x <- filter(x, duration > 0) %>%
#     split_events(at = "year", rule = "cut")
#
#   .day <- function(x) as.numeric(format(x, "%j"))
#
#   # we need the complete sequence of years as factor levels
#   # otherwise this years would be omitted in the plot
#   year <- format(x$start, "%Y")
#   x$year <- factor(year, levels = full_seq(as.numeric(year), 1))
#
#   # recode stard and end as day of the year (julian day)
#   # slightly incorrect for leap years...
#   x$start <- .day(x$start)
#   x$end <- .day(x$end)
#   x$end[x$end == 1] <- 366
#
#   # position labels months
#   breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 367)
#   at <- (head(breaks, -1) + tail(breaks, -1) - 2) / 2
#
#   p <- ggplot(x) +
#     # todo: use geom_rect
#     geom_segment(aes(x = start, xend = end, y = year, yend = year,
#                      col = state), size = size) +
#     geom_vline(xintercept = breaks, col = "white", alpha = 0.5) +
#     geom_hline(yintercept = seq(1, nlevels(x$year), by = 2), col = "white", alpha = 0.2) +
#     labs(title = paste0("Stream-Flow Permanence (threshold = ",
#                         threshold, ")")) +
#     scale_x_continuous(breaks = at, labels = month.abb, expand = c(0, 0)) +
#     scale_y_discrete(expand = c(0, 0.5), drop = F) +
#     theme(panel.grid = element_blank(),
#           axis.ticks = element_blank(),
#           axis.title = element_blank())
#
#   if(label)
#   {
#     labels <- .label_noflow_events(x)
#
#     p <- p + geom_text(data = labels,
#                        mapping = aes(x = x, y = y, label = label),
#                        size = 2.5)
#   }
#
#   return(p)
# }
#


# plot_period <- function(x, type = c("ts", "distribution"))
#   # todo: duration is hardcoded
# {
#   type <- match.arg(type)
#
#   if(!"duration" %in% colnames(x)) stop("There is no column called 'duration'. Hardcoded... :(")
#   if(type == "distribution")
#   {
#     p <- ggplot(x, aes(duration)) + geom_density(trim = TRUE) + facet_wrap(~ state) +
#       scale_x_continuous() + geom_rug() +
#       labs(x = "Maximum Duration per Period (days)", title = "Distribution of Events")
#   }
#
#   if(type == "ts")
#   {
#     x <-  ungroup(x) %>%
#       complete(year, state)
#
#     p <- ggplot(x, aes(year, duration, col = state, group = state)) +
#       geom_point() + geom_line() +
#       expand_limits(y = 0) +
#       scale_y_continuous() +
#       labs(y = "Maximum Duration (days)")
#   }
#
#   return(p)
# }


plot_groups <- function(x)
{
  frame <- get_attr_smires(x, key = "group_interval")
  int <- get_attr_smires(x, "interval")
  int$minor_hday <- c(int$minor_hday, 366)

  frame <- frame %>% mutate(x = as.numeric(minor),
                            y = as.numeric(major),
                            xmin = int$minor_hday[x],
                            xmax = int$minor_hday[x + 1],
                            ymin = y - 0.5,
                            ymax = y + 0.5)

  p <- ggplot() +
    geom_rect(data = frame,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, col = "darkgrey") +
    scale_x_continuous(name = "Minor Interval",
                       breaks = (head(int$minor_hday, -1) + tail(int$minor_hday, -1))/2,
                       expand = c(0, 0)) +
    scale_y_continuous(name = "Major Interval",
                       breaks = seq_len(nlevels(frame$major)),
                       labels = levels(frame$major),
                       expand = c(0, 0)) +
    # labs(subtitle = "Boxes are groups, segments are events.") +
    geom_text(data = frame,
              aes(x = xmax - 2,  y = ymin + 0.1, label = group),
              size = 3, hjust = 1, vjust = 0, col = "darkgrey") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank())

  if("rescaled" %in% colnames(x)) {
    p <- p +
      geom_line(data = x,
                aes(x = hday, y = rescaled, group = major))
  }


  if("event" %in% colnames(x)) {
    # todo: only works if events are cutted
    threshold <- get_attr_smires(x, key = "threshold")
    rule <- get_attr_smires(x, key = "rule")
    dy <- 0.2
    segment <- get_attr_smires(x, "event_cut") %>%
      mutate(xmin = date2hday(start, start = int$major),
             # 366 must not overflow to 1
             xmax = date2hday(end-1, start = int$major) +1,
             ymin = as.numeric(major) - dy + 0.2,
             ymax = as.numeric(major) + dy + 0.2)

    label <- ungroup(x) %>%
      mutate(gstart = int$minor_hday[as.numeric(minor)],
             gend = int$minor_hday[as.numeric(minor) + 1],
             estart = date2hday(start, start = int$major),
             eend = date2hday(end, start = int$major),
             ystart = as.numeric(hydrological_year(start, start = int$major)) + dy,
             x = pmax(gstart, estart),
             y = as.numeric(major) + dy + 0.2)

    geom_seg <- if(rule == "end") {
      geom_text(data = subset(label, as.numeric(duration, unit = "days") >= 7),
                aes(x = eend - 2, y = y, label = event),
                size = 3, hjust = 1, vjust = 1.1, col = "white")
    } else {
      geom_text(data = subset(label, as.numeric(duration, unit = "days") >= 7),
                aes(x = estart + 2, y = y, label = event),
                size = 3, hjust = 0, vjust = 1.1, col = "white")
    }

    p <- p +
      geom_rect(data = segment,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                    fill = state)) +
      geom_segment(data = label,
                   aes(x = estart, xend = estart, y = ystart + 0.2 , yend = ystart-dy/2 + 0.2),
                   col = "white") +
      geom_seg +
      labs(title = paste0("Stream-Flow Permanence (threshold = ",
                          threshold, ")"))
  }

  return(p)
}
