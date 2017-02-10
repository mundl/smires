duration <- function(x)
{
  if(x[1, "period"] != "whole ts") .warn_multiperiod(x)

  x %>%
    group_by(event, period, state) %>%
    summarize(start = head(time, 1), end = tail(time, 1),
              duration = end - start + 1)

}

find_events <- function(x, threshold = 0.001, na.rm = TRUE, period = NULL)
{
  x %>%
    dry_events(threshold = threshold) %>%
    per(period = period, na.rm = na.rm) %>%
    duration()
}