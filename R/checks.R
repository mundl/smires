# repeatedly oberserving a low value?

.guess_deltat <- function(x)
{
  dt <- diff(x)
  dtMed <- median(dt)

  if(dtMed != min(dt) & dtMed <= 7)
    # this logic doesn't work or montly data, February can have 29 days
    stop("Unable to derive dt from time series due to gaps. ")

  units(dtMed) <- "days"

  dt <- if(dtMed == 1) {
    "daily"
  } else if(dtMed == 7) {
    "weekly"
  } else if(dtMed %in% 30:31) {
    "monthly"
  } else {
    stop("Unable to derive dt from time series, median dt is probably larger than 31 days.")
  }

  return(dt)
}

# just checking the index, ignoring discharges which are NA
.missing_indices <- function(x)
{
  d <- diff(x)
  sum(d < median(d))
}

.msg_ratio <- function(value, total, text)
{
  if(value) {
    perc <- round(value / total, 3) * 100
    perc <- if(perc < 0.01) "< 0.01" else perc
    warning("The time series contains ", value, " ", text, " (", perc, " %).",
            sep = "", call. = FALSE)
  }
}

check_ts <- function(x, threshold = 0)
{
  # remove complete duplicates and order time series
  x <- unique(x)
  x <- x[order(x$time), ]

  dt <- .guess_deltat(x$time)
  attr(x, "dt") <- dt

  total <- nrow(x)
  .msg_ratio(sum(duplicated(x$time)), total, text = "duplicated indices")
  # todo: print first 6 missing indices
  .msg_ratio(.missing_indices(x$time), total,
             text = "missing indices, it is not regular")
  .msg_ratio(sum(is.na(x$value)), total, text = "missing observations")

  nthres <- sum(abs(x$value - threshold) < sqrt(.Machine$double.eps),
                na.rm = TRUE)
  txt <- paste0("observations equal to the threshold of '", threshold, "'")
  .msg_ratio(nthres, total, txt)

  txt <- paste0("observations below the threshold of '", threshold, "'")
  .msg_ratio(sum(x$value < threshold, na.rm = TRUE), total, txt)

  return(x)
}