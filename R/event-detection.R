find_events <- function(x, threshold = 0.001,
                        rule = c("cut", "duplicate", "start", "end"),
                        na.rm = TRUE, warn = TRUE)
{
  rule <- match.arg(rule)

  x %>%
    .detect_noflow_events(threshold = threshold) %>%

    .add_eventvars(warn = warn, duplicate = rule != "cut") %>%
    assign_event(rule = rule) %>%
    arrange(event) # sort by event
}


assign_event <- function(x, rule = c("cut", "duplicate", "start", "end"))
{
  rule <- match.arg(rule)
  x <- set_attr_smires(x, "rule", rule)

  # todo: rules for "majority" and "center"

  # events are already cut or duplicated
  if(rule %in% c("cut", "duplicate")) return(x)

  if(rule == "start") {
    y <- arrange(ungroup(x), event, group) %>%
      distinct(event, .keep_all = TRUE)
  }

  if(rule == "end") {
    y <- arrange(ungroup(x), desc(event), desc(group)) %>%
      distinct(event, .keep_all = TRUE) %>%
      arrange(event)
  }

  return(y)
}


.detect_noflow_events <- function(x, threshold = 0.1)
{
  if(is.null(threshold))
  {
    x$event <- seq_len(nrow(x))
  } else {
    att <- get_attr_smires(x)

    x$state <- ifelse(x$discharge <= threshold, "no-flow", "flow")
    x$state <- factor(x$state, levels = c("no-flow", "flow"))
    x <- mutate(x, event = .event(x$state))

    att[["threshold"]] <- threshold
    x <- set_attr_smires(x, value = att)
  }


  return(x)
}



.add_eventvars <- function(x, warn = TRUE, duplicate = FALSE)
{
  grouped <- "group" %in% colnames(x)

  y <- if(grouped && !duplicate) {
    group_by(x, event, state, group)
  } else {
    group_by(x, event, state)
  }

  att <- get_attr_smires(x)

  # always store cutted events in attributes,  needed for plotting
  if(grouped) {
    cut <- group_by(x, event, state, group)
  } else{
    cut <- group_by(x, event, state)
  }
  cut <- cut %>%
    summarize(start = min(time), end = max(time) + att$dt,
              duration = end - start)

  if(duplicate) {
    res <- y %>% do(data.frame(start = min(.$time), end = max(.$time) + att$dt,
                               group = unique(.$group))) %>%
      mutate(duration = end - start)
  } else {
    res <- summarize(y, start = min(time), end = max(time) + att$dt,
                     duration = end - start)
  }

  if(grouped) {
    # merge with minor an major intervals, if data was grouped
    res <- right_join(res, att[["group_interval"]], by = "group")
    cut <- right_join(cut, att[["group_interval"]], by = "group")
  }

  # quick and dirty way to drop smires attributes, no need to store them twice
  att[["event_cut"]] <- cut[, seq_along(cut)]

  #if(grouped | duplicate)
  res <- set_attr_smires(res, value = att)


  return(res)
}
#
# .duplicate_events <- function(x)
# {
#   # should always have exactly one row
#   if(nrow(x) != 1) browser()
#
#   # assumes there are columns group and group2 containing the groups
#   g <- c(x$group, x$group2)
#   n <- diff(g) + 1
#
#   y <- select(x, -group2) %>% slice(rep(1, n))
#   y$group <- seq(from = g[1], to = g[2])
#
#   return(y)
# }
#
# split_events <- function(x, at = c("year", "chunk"),
#                          rule = c("cut", "duplicate", "start", "end", "majority"))
# {
#   at <- match.arg(at)
#   rule <- match.arg(rule)
#   if(rule != "cut") warning("When splitting events, currently only method = 'cut' is supported.")
#
#   int <- attr(x, "interval")[[at]]
#
#   # actually, no grouping would be necessary...
#   x <- group_by(x, event) %>% do(.split_event(., int = int))
#
#   return(x)
# }
#
# .split_event <- function(x, int)
# {
#   if(!"year" %in% colnames(x))
#     stop("Splitting events before grouping doesn't make sense.")
#
#   if(!is.interval(int))
#     stop("Argument 'interval' must be of class interval.")
#
#   if(nrow(x) > 1) {
#     cat("should just have one row... :-(")
#     browser()
#   }
#
#   event <- interval(x$start, x$end)
#   int <- int[int_overlaps(int, event)]
#
#   if(length(int) == 1) return(x)
#
#   int_start(int[1]) <- x$start
#   int_end(int[length(int)]) <- x$end
#
#   dur <- as.numeric(int, unit = "days")
#
#   newdata <- tibble(event = x$event[1],
#                     start = as.Date(int_start(int)),
#                     end = as.Date(int_end(int)),
#                     duration = as.difftime(dur, units = "days"),
#                     year = factor(format(int_start(int), "%Y"),
#                                   levels = levels(x$year), ordered = TRUE))
#
#   select(x, -start, -end, -duration, -year) %>%
#     right_join(newdata, by = "event")
# }


.event <- function(x, new.group.na = TRUE, as.factor = TRUE)
{
  # copied from lfstat group()
  # operates just on the grouping variable

  x <- as.numeric(as.factor(x))

  if(!new.group.na) {
    s <- seq_along(x)
    finite <- !is.na(x)
    x <- approx(s[finite], x[finite], xout = s, f = 0,
                method = "constant", rule = c(1, 2))$y
  } else {
    # treat NAs as a group of its own
    # there isn't yet a level zero, therefore NAs can become zeros
    x[is.na(x)] <- 0
  }

  inc <- diff(x)
  if (new.group.na) inc[is.na(inc)] <- Inf

  grp <- c(0, cumsum(inc != 0))

  if(grp[1] == 0) grp <- grp + 1

  if(as.factor) {
    return(factor(grp, ordered = TRUE))
  } else {
    return(grp)
  }
}

