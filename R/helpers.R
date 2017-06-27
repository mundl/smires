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

# .factor_fullseq <- function(x, prefix = "", ordered = TRUE)
# {
#   fseq <- full_seq(x, 1)
#
#   factor(x, levels = fseq, labels = paste0(prefix, fseq), ordered = ordered)
# }


# replace with lubridate::yday()?
.date2julian <- function(x)
{
  if (is.instant(x)) {
    x <- as.numeric(format(as.Date(x), "%j"))
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


# to keep track of the variables
set_vars <- function(x, value)
{
  bad <- setdiff(value, colnames(x))
  if(length(bad)) {
    stop("The following variables are not present: ", paste(bad, collapse = ", "))
  }
  x <- set_attr_smires(x, "var") <- value
  return(x)
}

`set_vars<-` <- function(x, value)
{
  set_vars(x = x, value = value)
}

get_vars <- function(x)
{
  return(get_attr_smires(x, "var"))
}


# setting and retaining all smires attributes
get_attr_smires <- function(x, key = NULL)
{
  att <- as.list(attr(x, "smires"))
  if(is.null(key)) return(att)

  y <- att[[key]]
  if(is.null(y)) stop("Attribute '", key, "' does not exist.")

  return(y)
}

set_attr_smires <- function(x, key = NULL, value)
{
  att <- get_attr_smires(x)
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





format_jday <- function(x)
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

.ordinal_suffix <- function(x) {
  unit.pos <- x %% 10

  # suffix for values > 10 has to be th
  decade <- (x %% 100) %/% 10

  suffix <- rep_len("th", length(x))
  mask <- unit.pos %in% 1:3 & decade != 1
  suffix[mask] <- c("st", "nd", "rd")[unit.pos[mask]]

  return(suffix)
}


date2hday <- function(x, start)
{

  if(is.instant(x)) x <- yday(x)
  d <- x - start + 1
  hday <- ifelse(d %% 366 == 0, 365, ((d-1) %% 365 ) + 1 )

  return(hday)
}

# `[.smires`  <- function (x, i, j, ...) {
#   y <- NextMethod()
#   y <- `[`(y, i , j, ...)
#   y <- set_attr_smires(y, value = get_attr_smires(x))
#
#   class(y) <- class(x)
#   return(y)
# }

.rescale <- function(x, na.rm = TRUE)
{
  (x - min(x, na.rm = na.rm))/diff(range(x, na.rm = na.rm))
}
