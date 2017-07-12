# assign_period <- function(x, interval = NULL, include.year = TRUE, start = 1,
#                           span = FALSE)
# {
#   start <- .date2julian(start)
#
#   if("event" %in% colnames(x)) {
#     time <-   x$start
#     last <- x$end[nrow(x)] - 1
#   } else
#   {
#     time <- x$time
#     last <- tail(time, 1)
#   }
#
#   if(is.null(interval) || pmatch(interval, table = "years", nomatch = F)){
#     interval <- NULL
#     include.year <- TRUE
#   }
#
#   if(include.year){
#     int <- .year_interval(time, start = start, last = last)
#     allyears <- attr(int, "levels")
#
#     if(span) x$year.span <- int
#     y <- year(int_start(int))
#     x$year <- factor(y, levels = year(int_start(allyears)), ordered = TRUE)
#
#     attr(x, "interval")$year <- allyears
#   }
#
#
#   if(!is.null(interval)){
#     if(any(mday(int_start(allyears)) != 1))
#       stop("When using subyearly intervals, all years have to start on the",
#            "first day of a month.")
#
#     int <- .subyearly_interval(time, interval = interval, last = last)
#     allchunks <- attr(int, "levels")
#     if(span) x$interval.span <- int$interval
#     x$interval <- int$label
#
#     # match doesn't work on intervals
#     x$chunk <- match(int_start(int$interval), int_start(allchunks))
#
#     attr(x, "interval")$chunk <- allchunks
#   }
#
#   # todo: reorder factor when start != 1
#
#   return(x)
# }
#
# .subyearly_interval <- function(x, interval, last = max(x), as.interval = TRUE, prefix = TRUE)
# {
#   choices = c("weeks", "months", "quarters")
#   if(is.character(interval)) {
#     interval <- match.arg(interval, choices = choices)
#
#     label <- switch(interval,
#                     weeks = .factor_fullseq(as.numeric(format(x, "%U")),
#                                             prefix = if(prefix) "w" else ""),
#                     months = month(x, label = prefix),
#                     quarters = .factor_fullseq(quarter(x),
#                                                prefix = if(prefix) "q" else ""))
#
#     start <- floor_date(x, unit = interval)
#     # todo period(units = "quarters") doesnt work
#     if(as.interval) start <- as.interval(period(1, units = interval), start)
#   } else if(is.numeric(interval) || is.instant(interval)) {
#
#     start <- .date2julian(interval)
#     # todo: numeric or date -> season
#
#     warning("Seasons are not yet supported. ")
#     return()
#   } else {
#     stop("Argument `interval` must be either a date, an integer inside [1, 365]",
#          " or one in: ", paste(sQuote(choices), collapse = ", "))
#   }
#
#   y <- list(interval = start, label = label)
#   lvls <- seq(from = x[1], to = last, by = interval)
#   attr(y, "levels") <- as.interval(period(1, units = interval), lvls)
#
#   return(y)
# }
#
#
# .year_interval <- function(x, start = 1, last = max(x))
# {
#   start <- .date2julian(start)
#
#   # make sure the sequence of year cover whole time series even when start > 1
#   rangeY <- rangeTs <- c(min(x), last)
#   yday(rangeY) <- start
#
#   # if 'start' (start of hyd year) > start of ts, we also need the previous year
#   if(rangeTs[1] < rangeY[1]) rangeY[1] <- rangeY[1] - years(1)
#
#   # rangeY[2] is the end of the last interval,
#   # make sure it covers last value of the time series
#   if(rangeY[2] < rangeTs[2]) rangeY[2] <- rangeY[2] + years(1)
#
#   # breaks to cut.POSIX() must also include the enpoint of last interval
#   # adding 2 years, this is a bad hack... ;)
#   startY <- seq(rangeY[1], rangeY[2], "years")
#   year <- cut(x, startY, include.lowest = T, right = F, ordered_result = T)
#
#   # cut.POSIX drops breaks > max(x)
#   year <- factor(year, levels = as.character(head(startY, -1)), ordered = TRUE)
#
#
#   if(any(is.na(year))) stop("Introduced NAs when attributing year.")
#
#   lvls <- as.interval(years(), as.Date(levels(year)))
#   year <- lvls[as.numeric(year)]
#   attr(year, "levels") <- lvls
#
#   return(year)
# }
#

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

# mon <- yday(ymd(010101) + months(0:11))
# names(mon) <- month.abb
# dput(mon)

# week <- (0:51) * 7 + 1
# names(week) <- paste0("w", 1:52)
# dput(week)


intervals <- list(
  week = structure((0:51) * 7 + 1, .Names = paste0("w", 1:52)),

  month = structure(c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
                    .Names = month.abb),

  twoSeasons = structure(c(60, 244), .Names = c("summer", "winter")),

  fourSeasons = structure(c(60, 152, 244, 335),
                          .Names = c("spring", "summer", "autumn", "winter")))


twoSeasons <- c("summer" = as.Date("2015-03-01"),
                "winter" = as.Date("2015-09-01"))

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

  day <- .date2julian(x)
  names(day) <- n

  return(day)
}

# # todo: multiperiod stuff doesn't work anymore
# multiperiod <- function(x, verbose = FALSE)
# {
#   span <- tapply(x$period, x$event, function(x) length(unique(x)))
#   multi <- span[span > 1]
#   if(verbose)
#   {
#     return(x[x$event %in% multi, ])
#   }
#
#   m <- split(names(multi), multi)
#   class(m) <- "multiperiod"
#
#   return(m)
# }
#
# print.multiperiod <- function(x, ...)
# {
#   if(length(x) == 0)
#   {
#     cat("There are no multiperiod events.")
#     return(invisible())
#   }
#   events <- sapply(x, paste, collapse = ", ")
#   cat("Events spanning", names(events), "periods:", events)
# }
#
# .warn_multiperiod <- function(x)
# {
#   events <- unlist(multiperiod(x))
#   n <- length(events)
#   if(n)
#   {
#     message("The following ", .nplural(n, "event"), ngettext(n, " is", " are"),
#             " spanning multiple periods: ", .nmax(events))
#   }
# }

drop_na_periods <- function(x, period = group_vars(x))
{

  period <- setdiff(period, c("state", "discharge"))
  if(length(period) == 0) {
    warning("Not grouped by period.")
    return(x)
  }

  period <- match.arg(arg = period,
                      choices = c("group", "minor", "major"),
                      several.ok = TRUE)

  if(!period %in% colnames(x)) stop("Period not present in data.")

  # todo: use get_vars()
  # todo: instead of deleting the events, set value to NA to visualise missingness in plot
  var <- intersect(colnames(x), c("state", "discharge"))
  nas <- x[is.na(x[, var]), period]

  # todo: anti_joins reverses row order...
  y <- suppressMessages(anti_join(x, nas))

  return(y)
}

#
# drop_na_periods <- function(x, ..., col = "state") {
#   # todo: has to work without events
#   e <- sort(unique(x$event))
#   x <- group_by(x, ...) %>% do(.drop_na_period(., col = col))
#   rm <- setdiff(e, sort(unique(x$event)))
#
#   if(length(rm))
#     # todo: improve message. We do not remove events, but whole periods containing events.
#     message("Removing ", .nplural(length(rm), "event"),
#             " containing missing data. Event: ", .nmax(rm))
#
#   if(nrow(x) == 0)
#     warning("No records remaining after removing chunks with ",
#             "missing values in the specified columns. ",
#             "Try using a shorter periods, e.g. 'months'.")
#   return(x)
# }
#
# .drop_na_period <- function(x, col = "state") {
#   if(any(is.na(x[[col]]))) return(x[numeric(), ]) else return(x)
# }
#

agg_period <- function(period = c("none", "week", "month", "season", "year"),
                       start = 1) {

  # no aggregation needed: NULL
  # numeric & length() == 1: hydrological years
  # numeric & length() > 1 & named: seasons
  # numeric & length() == 12: months
  # numeric & length() == 52: weeks

  period <- match.arg(period)

  if(period == "none")
  {
    begin <- NULL
  } else if(period == "week") {
    begin <- interval$week
  } else if(period == "month") {
    begin <- interval$month
  }

  if(period %in% c("season", "year")) {
    # only for year and season, argument 'start' is relevant
    begin <- yday2(start)

    # check if argument start is of correct length
    if(period == "season") {
      if(length(start) < 2) {
        stop("When using 'period = \"season\"', length of argument 'start' must be > 2.")
      }
      begin <- start_season(start)

    } else {
      if(length(start) != 1) {
        stop("When using 'period = \"year\"', length of argument 'start' must be 1.")
      }
    }
  }

  return(begin)
}

correct_leapyear <- function(x)
{
  x <- as.Date(x)
  day <- yday(x)

  wrong <- .is_leapyear(x) & day >= 60
  if(any(wrong)) {
    # we are in a leap year and after Feb 28th
    warning("Date falls into a leap year. Using the previous day.")
    x[wrong] <- x[wrong] - 1
  }

  return(x)
}


yday2 <- function(x, correct_leapyear = TRUE)
{
  if(is.instant(x)) {
    x <- as.Date(x)

    if(correct_leapyear) x <- correct_leapyear(x)
    day <- yday(x)

  } else if(is.numeric(x) &&  x <= 365 && x > 0) {
    day <- x
  } else {
    stop("Argument 'x' must be eihter of class Date or an integer in [1, 365]")
  }

  return(day)
}


hydrological_year <- function(x, start = 1)
{
  x <- as.Date(x)
  start <- yday2(start)

  y <- year(x)
  j <- yday(x)
  mask <- j < start
  y[mask] <- y[mask] -1

  y <- factor(y, levels = full_seq(y, period = 1), ordered = TRUE)
  return(y)
}


group_by_interval <- function(.data, minor_interval = intervals$month,
                              major_interval = min(minor_interval))
{
  if (!is.numeric(major_interval) || length(major_interval) != 1) {
    stop("Currently, only years are supported for major intervals.")
  }

  if (is.numeric(major_interval) && is.numeric(minor_interval)) {
    idx <- match(major_interval, minor_interval)
    if(is.na(idx)) {
      candidate <- names(head(sort(abs(minor_interval - major_interval)), 2))
      stop("Major intervals must align with minor intervals.",
           " Minor interval does not contain: ", major_interval, "\n",
           "Consider choosing ", paste(shQuote(minor_interval[candidate]),
                                      collapse = " or "), ". ")
    }
  }

  int <- list(minor = minor_interval, major = major_interval)

  # major periods can only be years or hydrological years
  .data$major <- hydrological_year(.data$time, start = major_interval)


  # assign minor period
  day <- yday(.data$time)
  pos <- rowSums(outer(day, as.numeric(minor_interval), FUN = ">="))
  pos[pos == 0] <- length(minor_interval)
  n <- length(minor_interval)
  lvls <- names(minor_interval)[(seq(idx-1, length.out = n) %% n) + 1]
  .data$minor <- factor(names(minor_interval)[pos], levels = lvls, ordered = TRUE)

  int$minor <- int$minor[lvls]
  int[["minor_hday"]] <- date2hday(int$minor, start = major_interval)

  # update minor interval with correct order
  .data <- set_attr_smires(.data, "interval", int)

  # use integer arithmetic to get unique group numbers
  # todo: test if group_indices() is faster
  grp <- with(.data, as.numeric(minor) + as.numeric(major) * 100)
  .data$group <- as.numeric(factor(grp))

  # in leap years, feb 28th occurs twice
  .data$hday <- date2hday(.data$time, start = major_interval)

  # store interval in attributes
  tbl <- ungroup(.data) %>%
    select(major, minor, group) %>%
    distinct()
  .data <- set_attr_smires(.data, "group_interval", tbl)

  return(.data)
}


