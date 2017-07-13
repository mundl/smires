is.intermittent <- function(x, ndays = 5, consecutive = TRUE, threshold = 0.001)
{

  # shortcut
  if(all(na.omit(x$discharge) > threshold)) return(FALSE)

  e <- find_spells(x, threshold = threshold, na.rm = FALSE, warn = FALSE)
  e <- e[e$state == "no-flow" & !is.na(e$spell), ]

  days <- if(consecutive)
  {
    max(e$duration, na.rm = TRUE)
  } else {
    sum(e$duration, na.rm = TRUE)
  }

  return(days >= ndays)
}