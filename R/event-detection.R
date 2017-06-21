find_events <- function(x, threshold = 0.001, na.rm = TRUE, warn = TRUE)
{
  x %>%
    .detect_noflow_events(threshold = threshold) %>%
    .add_eventvars(warn = warn) %>%
    arrange(event) # sort by event
}

.detect_noflow_events <- function(x, threshold = 0.1)
{
  if(is.null(threshold))
  {
    x$event <- seq_len(nrow(x))
  } else {
    dt <- attr(x, "dt")

    x$state <- ifelse(x$discharge <= threshold, "no-flow", "flow")
    x$state <- factor(x$state, levels = c("no-flow", "flow"))
    x <- mutate(x, event = .event(x$state))

    attr(x, "threshold") <- threshold
    attr(x, "dt") <- dt
  }


  return(x)
}

.add_eventvars <- function(x, warn = TRUE)
{
  res <- x %>%
    group_by(event, state) %>%
    summarize(start = min(time), end = max(time) + dt,
                 duration = end - start)

  return(res)
}


split_events <- function(x, at = c("year", "chunk"),
                         rule = c("cut", "duplicate", "start", "end", "majority"))
{
  at <- match.arg(at)
  rule <- match.arg(rule)
  if(rule != "cut") warning("When splitting events, currently only method = 'cut' is supported.")

  int <- attr(x, "interval")[[at]]

  # actually, no grouping would be necessary...
  x <- group_by(x, event) %>% do(.split_event(., int = int))

  return(x)
}

.split_event <- function(x, int)
{
  if(!"year" %in% colnames(x))
    stop("Splitting events before grouping doesn't make sense.")

  if(!is.interval(int))
    stop("Argument 'interval' must be of class interval.")

  if(nrow(x) > 1) {
    cat("should just have one row... :-(")
    browser()
  }

  event <- interval(x$start, x$end)
  int <- int[int_overlaps(int, event)]

  if(length(int) == 1) return(x)

  int_start(int[1]) <- x$start
  int_end(int[length(int)]) <- x$end

  dur <- as.numeric(int, unit = "days")

  newdata <- tibble(event = x$event[1],
                    start = as.Date(int_start(int)),
                    end = as.Date(int_end(int)),
                    duration = as.difftime(dur, units = "days"),
                    year = factor(format(int_start(int), "%Y"),
                                  levels = levels(x$year), ordered = TRUE))

  select(x, -start, -end, -duration, -year) %>%
    right_join(newdata, by = "event")
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

