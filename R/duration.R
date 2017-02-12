duration <- function(x)
{
  if(x[1, "period"] != "whole ts") .warn_multiperiod(x)

  # mutate() drops attributes...
  threshold <- attr(x, "threshold")

  # manually add events of length  0
  # complete also completes grouping variables, regroup
  res <- x %>%
    group_by(event, period, pid, state) %>%
    summarize(start = head(time, 1), end = tail(time, 1) + 1) %>%
    mutate(duration = end - start) %>%
    ungroup() %>%
    complete(nesting(period), state, fill = list(duration = 0))

  attr(res, "threshold") <- threshold
  return(res)
}

find_events <- function(x, threshold = 0.001, na.rm = TRUE, period = NULL)
{
  x %>%
    dry_events(threshold = threshold) %>%
    per(period = period, na.rm = na.rm) %>%
    duration() %>%
    group_by(period, state) %>%
    arrange(event)
}