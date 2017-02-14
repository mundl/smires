is.intermittent <- function(x, ndays = 5, consecutive = TRUE, threshold = 0.001,
                            period = "year")
  # todo: find_event() must support water year.
{

  e <- find_events(x, threshold = threshold, na.rm = FALSE, period = period,
                   warn = FALSE)
  e <- e[e$state == "no-flow" & !is.na(e$event), ]

  days <- if(consecutive)
  {
    max(e$duration, na.rm = TRUE)
  } else {
    sum(e$duration, na.rm = TRUE)
  }

  return(days >= ndays)
}