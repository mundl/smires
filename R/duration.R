duration <- function(x, warn = TRUE)
{
  if(x[1, "period"] != "whole ts" & warn) .warn_multiperiod(x)

  # mutate() drops attributes...
  threshold <- attr(x, "threshold")

  # manually add events of length  0
  # complete also completes grouping variables, regroup
  res <- x %>%
    group_by(event, period, pid, state) %>%
    summarize(start = head(time, 1), end = tail(time, 1) + 1) %>%
    mutate(duration = end - start) %>%
    ungroup() %>%
    complete(nesting(period), state, fill = list(duration = 0)) %>%
    group_by(period, state)

  attr(res, "threshold") <- threshold
  return(res)
}

identity <- function(x, ...)
{
  if(x[1, "period"] != "whole ts") .warn_multiperiod(x)

  #just perform grouping
  res <- x %>%
    group_by(period)

  return(res)
}


find_events <- function(x, threshold = 0.001, na.rm = TRUE, period = NULL,
                        fun = duration, warn = TRUE)
{
  x %>%
    dry_events(threshold = threshold) %>%
    per(period = period, na.rm = na.rm) %>%
    fun(warn = warn) %>%
    arrange(event) # sort by event
}