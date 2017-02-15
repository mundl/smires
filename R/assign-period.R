assign_period <- function(x, interval = NULL, include.year = TRUE, start = 1,
                          span = FALSE)
{
  if("event" %in% colnames(x)) {
    time <-   x$start
    last <- x$end[nrow(x)]
  } else
  {
    time <- x$time
    last <- tail(time, 1)
  }

  if(include.year){
    int <- .year_interval(time, start = start, last = last)
    allyears <- attr(int, "levels")

    if(span) x$year.span <- int
    y <- year(int_start(int))
    x$year <- factor(y, levels = year(int_start(allyears)), ordered = TRUE)

    attr(x, "interval")$year <- allyears
  }

  if(!is.null(interval)){
    int <- .subyearly_interval(time, interval = interval)
    if(span) x$interval.span <- int$interval
    x$interval <- int$label
    x$chunk <- as.numeric(as.factor(int$interval))

    # todo: do not use unique() carry on from attributes
    attr(x, "interval")$chunk <- unique(int$interval)
  }

  # todo: check if intervals and years align

  return(x)
}

.subyearly_interval <- function(x, interval, as.interval = TRUE, prefix = TRUE)
{
  choices = c("weeks", "months", "quarters")
  if(is.character(interval)) {
    interval <- match.arg(interval, choices = choices)

    label <- switch(interval,
                    weeks = .factor_fullseq(as.numeric(format(x, "%U")),
                                            prefix = if(prefix) "w" else ""),
                    months = month(x, label = prefix),
                    quarters = .factor_fullseq(quarter(x),
                                               prefix = if(prefix) "q" else ""))

    start <- floor_date(x, unit = interval)
    if(as.interval) start <- as.interval(period(1, units = interval), start)
  } else if(is.numeric(interval) || is.instant(interval)) {

    # todo: numeric or date -> season

    warning("Seasons are not yet supported. ")
    return()
  } else {
    stop("Argument `interval` must be either a date, an integer inside [1, 365]",
         " or one in: ", paste(sQuote(choices), collapse = ", "))
  }

  return(list(interval = start, label = label))
}


.year_interval <- function(x, start = 1, last = max(x))
{
  if (is.instant(start)) {
    start <- as.numeric(format(as.Date(start), "%j"))
  }

  if(!is.numeric(start) || start < 1 || start > 365) {
    stop("Argument `start` must be either date or an integer inside [1, 365]. ")
  }

  # make sure the sequence of year cover whole time series even when start > 1
  rangeY <- rangeTs <- c(min(x), last)
  yday(rangeY) <- start

  # if 'start' (start of hyd year) > start of ts, we also need the previous year
  if(rangeTs[1] < rangeY[1]) rangeY[1] <- rangeY[1] - years(1)

  # rangeY[2] is the end of the last interval,
  # make sure it covers last value of the time series
  if(rangeY[2] < rangeTs[2]) rangeY[2] <- rangeY[2] + years(1)

  # breaks to cut.POSIX() must also include the enpoint of last interval
  # adding 2 years, this is a bad hack... ;)
  startY <- seq(rangeY[1], rangeY[2], "years")
  year <- cut(x, startY, include.lowest = T, right = F, ordered_result = T)

  # cut.POSIX drops breaks > max(x)
  year <- factor(year, levels = as.character(head(startY, -1)), ordered = TRUE)


  if(any(is.na(year))) stop("Introduced NAs when attributing year.")

  # todo: one year to much...
  lvls <- as.interval(years(), as.Date(levels(year)))
  year <- lvls[as.numeric(year)]
  attr(year, "levels") <- lvls

  return(year)
}


# per <- function(x, period, na.rm = TRUE)
# {
#   # manually add events of length  0
#   # complete also completes grouping variables, regroup
#
#
#   if(is.null(period))
#   {
#     x$period <- "whole ts"
#   } else {
#     time <- x$time
#     f <- c("week" = "%V", "month" = "%m", "year" = "%Y")
#
#     if (is.character(period))
#     {
#       period <- match.arg(arg = period, choices = names(f), several.ok = FALSE)
#       x$period <- as.numeric(format(time, format = f[period]))
#
#       x$period <- switch(period,
#                          week = factor(x$period, levels = 1:53),
#                          month = factor(x$period, levels = 1:12),
#                          year = factor(x$period, levels = full_seq(as.numeric(x$period), 1)))
#
#     } else if (is.numeric(period)) {
#       # seasonal
#       start <- sort(period)
#       int <- findInterval(as.numeric(format(time, "%j")), start)
#       int[int == 0] <- length(start)
#
#       x$period <- factor(names(start)[int], levels = names(start))
#
#     } else {
#       stop("Argument 'period' must be eihter numeric or one of: ",
#            paste(sQuote(names(f)), collapse = ", "))
#     }
#   }
#
#   # todo: find meaningful ids for periods: eg 2011-05, 2012-w45, 1985-s1
#   # or calculate period id (pid) in remove_na_periods()
#   x$pid <- .event(x$period, as.factor = FALSE)
#
#   if(na.rm) x <- remove_na_periods(x)
#
#   return(x)
# }

# todo: possible duplicated functionality in .year_interval()
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

# todo: multiperiod stuff doesn't work anymore
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
  if(length(x) == 0)
  {
    cat("There are no multiperiod events.")
    return(invisible())
  }
  events <- sapply(x, paste, collapse = ", ")
  cat("Events spanning", names(events), "periods:", events)
}

.warn_multiperiod <- function(x)
{
  events <- unlist(multiperiod(x))
  n <- length(events)
  if(n)
  {
    message("The following ", .nplural(n, "event"), ngettext(n, " is", " are"),
            " spanning multiple periods: ", .nmax(events))
  }
}

# todo: duplicated
# remove_na_periods <- function(x, col = "discharge")
# {
#   if(x[1, "period"] == "whole ts" && any(is.na(x$discharge)))
#   {
#     stop("Can't calculate events for the whole time series because ",
#          "discharges contain missing values. Try to group per 'year' or ",
#          "'month'. E.g. `period = 'month'`")
#     return(x[, ])
#   }
#
#   miss <- unique(x$pid[is.na(x[, col])])
#   n <- length(miss)
#
#   if(n)
#   {
#     x <- filter(x, !pid %in% miss)
#
#     message("Removing ", .nplural(n, "period"),
#             " containing missing data. pid: ", .nmax(miss))
#   }
#
#   return(x)
# }

drop_na_periods <- function(x, ..., col = "state") {
  group_by(x, ...) %>% do(.drop_na_period(., col = col))
}

.drop_na_period <- function(x, col = "state") {
  if(any(is.na(x[[col]]))) return(x[numeric(), ]) else return(x)
}
