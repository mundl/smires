is.intermittent <- function(x, ndays = 5, nyears = 1,
                            consecutive = c("none", "days", "years"),
                            threshold = 0.001)
{

  consecutive <- match.arg(consecutive, several.ok = TRUE)
  e <- find_events(x, threshold = threshold, na.rm = FALSE, period = "year",
                   warn = FALSE)
  e <- e[e$state == "dry" & !is.na(e$event), ]
  spell <- filter(e, duration >= ndays)

  intermittent <-  if (any(consecutive == "none"))
    {
    sum(e$duration, na.rm = TRUE) > ndays & length(unique(spell$period)) >= nyears
  } else if(all(consecutive == "days"))
  {
    length(unique(spell$period)) >= nyears
  } else if(all(consecutive == "years"))
  {
    rl <- rle(diff(as.numeric(as.character(unique(e$period)))))
    any(rl$lengths[rl$values == 1] >= nyears - 1)
  } else if(all(c("years", "days") %in% consecutive))
  {
    rl <- rle(diff(as.numeric(as.character(unique(spell$period)))))
    any(rl$lengths[rl$values == 1] >= nyears - 1)
  }

  return(intermittent)
}