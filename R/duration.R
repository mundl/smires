duration <- function(x)
{
  if(x[1, "period"] != "whole ts") .warn_multiperiod(x)

  # mutate() drops attributes...
  threshold <- attr(x, "threshold")

  # todo: include events of length  0
  res <- x %>%
    group_by(event, period, pid, state) %>%
    summarize(start = head(time, 1), end = tail(time, 1)) %>%
    mutate(duration = end - start + 1)

  attr(res, "threshold") <- threshold
  return(res)
}

find_events <- function(x, threshold = 0.001, na.rm = TRUE, period = NULL)
{
  x %>%
    dry_events(threshold = threshold) %>%
    per(period = period, na.rm = na.rm) %>%
    duration() %>%
    group_by(period, state)
}