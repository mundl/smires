.ordinal_suffix <- function(x) {
  unit.pos <- x %% 10

  # suffix for values > 10 has to be th
  decade <- (x %% 100) %/% 10

  suffix <- rep_len("th", length(x))
  mask <- unit.pos %in% 1:3 & decade != 1
  suffix[mask] <- c("st", "nd", "rd")[unit.pos[mask]]

  return(suffix)
}

.written_suffix <- function(x)
{
  if(x > 4) return(paste0(x, .ordinal_suffix(x)))
  switch(x,
         "1" = "first",
         "2" = "second",
         "3" = "third")
}


.rescale <- function(x, na.rm = TRUE)
{
  (x - min(x, na.rm = na.rm))/diff(range(x, na.rm = na.rm))
}


.nplural <- function(n, msg, suffix = "s")
{
  # prints number + text, appends an plural "s" if n > 1
  msg <- paste(n, msg)
  ngettext(n, msg, paste0(msg, suffix))
}


.nmax <- function(x, nmax = 6, suffix = ", ...", collapse = ", ")
{
  txt <- paste(head(x, nmax), collapse = collapse)
  if(length(x) > nmax) txt <- paste0(txt, suffix)

  return(txt)
}

.itemize_text <- function(x, collapse = ", ", final = " and ")
{
  paste(paste(head(x, -1), collapse = ", "),
        tail(x, 1), sep = " and ")
}

# .factor_fullseq <- function(x, prefix = "", ordered = TRUE)
# {
#   fseq <- full_seq(x, 1)
#
#   factor(x, levels = fseq, labels = paste0(prefix, fseq), ordered = ordered)
# }


# attributes of smires objects ----

# to keep track of the variables
# currently unused!
.set_vars <- function(x, value)
{
  bad <- setdiff(value, colnames(x))
  if(length(bad)) {
    stop("The follwring variables are not present: ", paste(bad, collapse = ", "))
  }
  x <- .set_attr_smires(x, key = "var", value = value)
  return(x)
}

`.set_vars<-` <- function(x, value)
{
  .set_vars(x = x, value = value)
}

.get_vars <- function(x)
{
  return(.get_attr_smires(x, "var"))
}


# setting and retaining all smires attributes
.get_attr_smires <- function(x, key = NULL)
{
  att <- as.list(attr(x, "smires"))
  if(is.null(key)) return(att)

  y <- att[[key]]
  if(is.null(y)) stop("Attribute '", key, "' does not exist.")

  return(y)
}

.set_attr_smires <- function(x, key = NULL, value)
{
  att <- .get_attr_smires(x)
  if(is.null(key)) {
    # todo: remove comment, is.intermittent()
    # if (length(att)) warning("Overwriting existing attributes.")
    attr(x, "smires") <- value
  } else {
    att[[key]] <- value
    attr(x, "smires") <- att
  }

  return(x)
}



# `[.smires`  <- function (x, i, j, ...) {
#   y <- NextMethod()
#   y <- `[`(y, i , j, ...)
#   y <- .set_attr_smires(y, value = .get_attr_smires(x))
#
#   class(y) <- class(x)
#   return(y)
# }


# date/time related functions ----

.date2hday <- function(x, start)
{

  if(is.instant(x)) x <- yday(x)
  d <- x - start + 1
  hday <- ifelse(d %% 366 == 0, 365, ((d-1) %% 365 ) + 1 )

  return(hday)
}


# currently unused
.format_jday <- function(x)
{
  nam <- names(x)
  if(is.numeric(x)) {
    if(all(x > 0 & x < 366)){
      x <- as.Date(x-1, origin = "1970-01-01")
    } else {
      stop("Argument `x` must be either date or an integer inside [1, 365].")
    }
  }

  x <- as.Date(x)
  month <- month.abb[as.numeric(format(x, "%m"))]

  day <- as.numeric(format(x, "%d"))
  suffix <- .ordinal_suffix(day)
  day <- paste0(day, suffix)

  y <- paste0(month, " ", day, " (day ", as.numeric(format(x, "%j")), ")")
  names(y) <- nam

  return(y)
}


julian_day <- function(x)
{
  if (is.instant(x)) {
    # correct leap years
    day <- as.numeric(format(as.Date(x), "%j"))
    wrong <- .is_leapyear(x) & day >= 60
    day[wrong] <- day[wrong] - 1
    x <- day
  }

  if(!is.numeric(x) || x < 1 || x > 365) {
    stop("Argument `x` must be either date or an integer inside [1, 365]. ")
  }

  return(x)
}

.is_leapyear <- function(x)
{
  year <- year(x)
  (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0))
}

.correct_leapyear <- function(x)
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


.yday2 <- function(x, correct_leapyear = TRUE)
{
  if(is.instant(x)) {
    x <- as.Date(x)

    if(correct_leapyear) x <- .correct_leapyear(x)
    day <- yday(x)

  } else if(is.numeric(x) &&  x <= 365 && x > 0) {
    day <- x
  } else {
    stop("Argument 'x' must be eihter of class Date or an integer in [1, 365]")
  }

  return(day)
}

.circular_stats <- function(x, lwr = 0, upr = 365) {
  if(any(x < lwr | x > upr))
    stop("input data not in range [", lwr, ", ", upr, "]")

  ang <- (x - lwr)/upr * 2 *pi

  m <- mean(exp(1i * ang))  # mean vector
  a <- Mod(m)               # absolute value

  phi <- Arg(m)
  if (phi < 0) phi <- Arg(m) + 2*pi
  cm <- phi * (upr - lwr)/2/pi + lwr


  cv <- ((upr - lwr)/2/pi)^2 * 2 * log(1/a)

  csd <- ((upr - lwr)/2/pi) * sqrt(-2* log(a))

  return(c(mean = cm, var = cv, sd = csd, abs = a))
}

circular_mean <- function(x, lwr = 0, upr = 365)
  .circular_stats(x = x, lwr = lwr, upr = upr)["mean"]

circular_r <- function(x, lwr = 0, upr = 365)
  .circular_stats(x = x, lwr = lwr, upr = upr)["abs"]

# circular_cv <- function(x, lwr = 0, upr = 365){
#   y <- .circular_stats(x = x, lwr = lwr, upr = upr)
#   return(y["sd"]/y["mean"])
# }

mean_day <- function(x, lwr = 0, upr = 365)
{
  .format_jday(circular_mean(x = x, lwr = lwr, upr = upr))
}

