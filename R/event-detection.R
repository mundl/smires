find_events <- function(x, threshold = 0.001, na.rm = TRUE, warn = TRUE)
{
  x %>%
    detect_noflow_events(threshold = threshold) %>%
    add_eventvars(warn = warn) %>%
    arrange(event) # sort by event
}

detect_noflow_events <- function(x, threshold = 0.1)
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

add_eventvars <- function(x, warn = TRUE)
{
  # mutate() drops attributes...
  threshold <- attr(x, "threshold")
  dt <- attr(x, "dt")

  # use mutate to keep class difftime
  res <- x %>%
    group_by(event, state) %>%
    do(summarize(., start = min(time), end = max(time) + dt)) %>%
    mutate(duration = end - start)

  attr(res, "threshold") <- threshold
  attr(res, "dt") <- dt
  return(res)
}


split_event <- function(x, rule = c("duplicate", "start", "end", "majority"))
{
  rule <- match.arg(rule)
  warning("Splitting events is currently not supported.")

  return(x)
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

