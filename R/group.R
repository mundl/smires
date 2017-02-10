# Grouping per period (year, month, season) ----
per <- function(x, period, na.rm = TRUE)
{
  time <- x$time
  f <- c("week" = "%V", "month" = "%m", "year" = "%Y")

  if (is.character(period))
  {
    period <- match.arg(arg = period, choices = names(f), several.ok = FALSE)
    x$period <- as.numeric(format(time, format = f[period]))
  } else if (is.numeric(period)) {
    # seasonal
    start <- sort(period)
    int <- findInterval(as.numeric(format(time, "%j")), start)
    int[int == 0] <- length(start)

    x$period <- names(start)[int]

  } else {
    stop("Argument 'period' must be eihter numeric or one of: ",
         paste(sQuote(names(f)), collapse = ", "))
  }

  # todo: find meaningful ids for periods: eg 2011-05, 2012-w45, 1985-s1
  # or calculate period id (pid) in remove_na_periods()
  x$pid <- .event(x$period, as.factor = FALSE)

  if(na.rm) x <- remove_na_periods(x)

  return(x)
}

start_season <- function(x)
{
  n <- names(x)

  if(is.null(n)) {
    n <- paste0("S", seq_along(x))
    warning("Seasons should have meaningful, unambiguous names.")
  }

  if(anyDuplicated(names(x)) | "" %in% names(x)) {
    n <- make.names(names(x), unique = TRUE)
    warning("Seasons should have meaningful, unambiguous names.")
  }

  day <- as.numeric(format(as.Date(x), "%j"))
  names(day) <- n

  return(day)
}

multiperiod <- function(x, verbose = FALSE)
{
  span <- tapply(x$period, x$event, function(x) length(unique(x)))
  multi <- span[span > 1]
  if(verbose)
  {
    return(x[x$event %in% multi, ])
  }

  m <- split(names(multi), multi)
  class(m) <- "multiperiod"

  return(m)
}

print.multiperiod <- function(x, ...)
{
  events <- sapply(x, paste, collapse = ", ")
  cat("Events spanning", names(events), "periods:", events)
}

.warn_multiperiod <- function(x)
{
  events <- unlist(multiperiod(x))
  n <- length(events)
  message("The following ", .nplural(n, "event"), ngettext(n, " is", " are"),
          " spanning multiple periods: ", .nmax(events))
}

# Detect events ----
dry_events <- function(x, threshold = 0.1)
{
  x$state <- ifelse(x$value <= threshold, "dry", "wet")
  x <- .find_events(x)
  attr(x, "threshold") <- threshold

  return(x)
}

# .replace_na <- function(x, na = c("wet", "dry"))
# {
#   na <- match.arg(na)
#
#   # when calculating durations, NAs can be assumed to be
#   # no flow periods or flow periods
#   x$value[is.na(x$value)] <- if(na == "flow") Inf else 0
#
#   return(x)
# }

.nplural <- function(n, msg, suffix = "s") {
  msg <- paste(n, msg)
  ngettext(n, msg, paste0(msg, suffix))
}


.nmax <- function(x, nmax = 6, suffix = ", ...", collapse = ", ")
{
  txt <- paste(head(x, nmax), collapse = collapse)
  if(length(x) > nmax) txt <- paste0(txt, suffix)

  return(txt)
}


remove_na_periods <- function(x, col = "value")
{
  miss <- unique(filter(x, is.na(state)) %>% .[["pid"]])
  n <- length(miss)

  y <- filter(x, !pid %in% miss)

  message("Removing the following ", .nplural(n, "period"),
          " because they contain missing data: ", .nmax(miss))

  return(y)
}

.find_events <- function(x)
{
  # operates on the data.frame
  stopifnot(all(x$state %in% c("wet", "dry", NA)))
  data.frame(x, event = .event(x$state))
}

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
    return(factor(grp))
  } else {
    return(grp)
  }
}
