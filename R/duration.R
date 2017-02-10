duration <- function(x)
{
  if(!"period" %in% colnames(x))
  {
    x$period <- "whole ts"
  } else {
    .warn_multiperiod(x)
  }

  x %>%
    group_by(event, period, state) %>%
    summarize(start = head(time, 1), end = tail(time, 1),
              duration = end - start + 1)

}
