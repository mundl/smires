# Grouping per period (year, month, season) ----
per_period <- function(x, per)
{
  time <- x$time
  f <- c("week" = "%V", "month" = "%m", "year" = "%Y")

  if (is.character(per))
  {
    per <- match.arg(arg = per, choices = names(f), several.ok = FALSE)
    x$period <- as.numeric(format(time, format = f[per]))
  } else if (is.numeric(per)) {
    # seasonal
    start <- sort(per)
    int <- findInterval(as.numeric(format(time, "%j")), start)
    int[int == 0] <- length(start)

    x$period <- names(start)[int]

  } else {
    stop("Argument 'per' must be eihter numeric or one of: ",
         paste(sQuote(names(f)), collapse = ", "))
  }

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
  n <- length(unlist(multiperiod(x)))
  warning("There are ", n, " events spanning multiple periods.",
          call. = FALSE)
}

# Detect events ----
dry_events <- function(x, na, threshold = 0.1)
{
  if(any(is.na(x$value))) {
    x <- .replace_na(x = x, na = na)
  }

  x$state <- ifelse(x$value <= threshold, "dry", "wet")

  return(.find_events(x))
}

.replace_na <- function(x, na = c("wet", "dry"))
{
  na <- match.arg(na)

  # when calculating durations, NAs can be assumed to be
  # no flow periods or flow periods
  x$value[is.na(x$value)] <- if(na == "flow") Inf else 0

  return(x)
}

.find_events <- function(x)
{
  # operates on the data.frame
  stopifnot(all(x$state %in% c("wet", "dry")))
  data.frame(x, event = .event(x$state))
}

.event <- function(x, new.group.na = TRUE, as.factor = TRUE)
{
  # operates just on the grouping variable
  if(!new.group.na) {
    s <- seq_along(x)
    finite <- !is.na(x)
    x <- approx(s[finite], x[finite], xout = s, f = 0,
                method = "constant", rule = c(1, 2))$y
  }

  inc <- diff(as.numeric(as.factor(x)))
  if (new.group.na) inc[is.na(inc)] <-  Inf

  grp <- c(0, cumsum(inc != 0))

  if(grp[1] == 0) grp <- grp + 1

  if(as.factor) {
    return(factor(grp))
  } else {
    return(grp)
  }
}
