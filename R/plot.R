plot_groups <- function(x)
{
  frame <- .get_attr_smires(x, key = "group_interval")
  int <- .get_attr_smires(x, "interval")
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
    # labs(subtitle = "Boxes are groups, segments are spells.") +
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


  if("spell" %in% colnames(x)) {
    # todo: only works if spells are cutted
    threshold <- .get_attr_smires(x, key = "threshold")
    rule <- .get_attr_smires(x, key = "rule")
    dy <- 0.2
    segment <- .get_attr_smires(x, "spell_cut") %>%
      mutate(xmin = .date2hday(onset, start = int$major),
             # 366 must not overflow to 1
             xmax = .date2hday(termination-1, start = int$major) +1,
             ymin = as.numeric(major) - dy + 0.2,
             ymax = as.numeric(major) + dy + 0.2)

    label <- ungroup(x) %>%
      mutate(gstart = int$minor_hday[as.numeric(minor)],
             gend = int$minor_hday[as.numeric(minor) + 1],
             estart = .date2hday(onset, start = int$major),
             eend = .date2hday(termination, start = int$major),
             ystart = as.numeric(hydrological_year(onset, start = int$major)) + dy,
             x = pmax(gstart, estart),
             y = as.numeric(major) + dy + 0.2)

    geom_seg <- if(rule == "termination") {
      geom_text(data = subset(label, as.numeric(duration, unit = "days") >= 7),
                aes(x = eend - 2, y = y, label = spell),
                size = 3, hjust = 1, vjust = 1.1, col = "white")
    } else {
      geom_text(data = subset(label, as.numeric(duration, unit = "days") >= 7),
                aes(x = estart + 2, y = y, label = spell),
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
      scale_fill_discrete(drop = F) +
      labs(title = paste0("Stream-Flow Permanence (threshold = ",
                          threshold, ")"))
  }

  return(p)
}


plot_intermittency <- function(x, ...)
{
  null <- smires(x, ..., plot = TRUE)
  return(invisible())
}

# todo: write a functions() plot_spell that works for ungrouped events
# or make plot_groups() compatible with ungrouped events --> better name?