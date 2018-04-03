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
  if(na.rm) x[!is.finite(x)] <- NA
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

.insert_at_pos <- function(x, after, values) {
  x <- strsplit(x, split = "", fixed = TRUE)[[1]]
  for(pos in sort(after, decreasing = T)){
    x <- append(x = x, values = values, after = pos)
  }

  return(paste(x, collapse = ""))
}

filecontent <- function(file, nrow = 6, skip = 0,
                        ndigits = ceiling(log10(nrow+skip+1)), linenumber = TRUE,
                        width = options()$width, sep = "|  ", ...)
{
  con <- file(file, ...)
  open(con)

  if(skip > 0) x <- readLines(con, n = skip)
  x <- readLines(con, n = 10)

  close(con)

  # make it 1 smaller than necessary
  nmax <- width - nchar(sep) - ndigits - 10

  longline <- which(nchar(x) > nmax)
  for(i in longline) {
    pos <- seq(from = nmax, to = nchar(x[i]), by = nmax)
    x[i] <- .insert_at_pos(x[i], after = pos,
                           values = paste0(rep("\n  ", ndigits), sep))
  }

  linenumber <- format(seq_along(x)+skip, justify = "right")


  x <- paste(linenumber, x, sep = sep)
  x <- gsub("\t", " -> ", x) #"\u2B7E"
  cat(x, sep = "\n")
  return(invisible(x))
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
  attr_smires(x) <- list("var" = value)
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

.drop_dt <- function(x)
{
  x[setdiff(names(x), "dt")]
}

attr_smires <- function(x, as.sf = FALSE)
{
  att <- attr(x, "smires")
  if(is.null(att)) {
    if(inherits(x, what = "list")) {
      # if its is a list, check if all elements have attributes
      l <- lapply(x, function(x) .drop_dt(attr_smires(x)))

      # bind_rows() cannot handle mixed types, convert to character first
      char <- lapply(l, function(x) as_data_frame(lapply(x, as.character)))

      # bind_rows() cannot handle element with length == 0
      char <- lapply(char, function(x) {x[, lengths(x) == 0] <- NA; x})

      ll <- bind_rows(char)
      ll <- mutate_all(ll, type.convert, as.is = TRUE)

      station <- bind_rows(ll)
      if(as.sf) station <- .attr_as_sf(station)

      return(station)

    } else {
      return(NULL)
      #stop("Object '", deparse(substitute(x)), "' has no smires attributes.")
    }
  }
  return(att)
}

`attr_smires<-` <- function(x, value)
{
  if(!is.list(value) || any(is.na(names(value)) | names(value) == "")) {
    stop("Argument 'value' must be a named list.")
  }

  if(inherits(x, what = "list")) {
    # if its is a list, set the attributes for all elements
    l <- lapply(x, `attr_smires<-`, value = value)
    return(l)
  }

  att <- attr_smires(x)
  att[names(value)] <- value

  attr(x, "smires") <- att
  return(x)
}


# setting and retaining all smires attributes
.get_attr_smires <- function(x, key = NULL)
{
  att <- attr_smires(x)
  if(is.null(key)) return(att)

  y <- att[[key]]
  if(is.null(y)) stop("Attribute '", key, "' does not exist.")

  return(y)
}

# `[.smires`  <- function (x, i, j, ...) {
#   y <- NextMethod()
#   y <- `[`(y, i , j, ...)
#   y <- .set_attr_smires(y, value = .get_attr_smires(x))
#
#   class(y) <- class(x)
#   return(y)
# }

.attr_as_sf <- function(x)
{
  if(!all(c("lon", "lat") %in% colnames(x))) {
    warning("Object '", deparse(substitute(x)), "' does not contain coordinates.")
    return(x)
  }

  y <- st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
  y$lon <- x$lon
  y$lat <- x$lat
  return(y)
}

enframe_smires <- function(x, as.sf = FALSE)
{
  x <- assign_ids(x)
  att <- attr_smires(x, as.sf = as.sf)
  data <- enframe(x, name = "sid", value = "data")


  # print important colnames first
  cnames <- c("sid", "data",
              setdiff(colnames(att), c("sid", "filename", "dirname")),
              "filename", "dirname")

  y <- right_join(att, data, by = "sid")[, cnames]
  return(y)
}

deframe_smires <- function(x)
{
  att.cols <- setdiff(colnames(x), "data")
  if (length(att.cols) == 0) return(x)

  if (!"data" %in% colnames(x)) stop("must have a column named 'data'.")

  for (i in seq_len(nrow(x))) {
    attr_smires(x$data[[i]]) <- as.list(x[i, att.cols])
  }

  return(x)
}



# date/time related functions ----

.date2hday <- function(x, start)
{

  if(is.instant(x)) x <- yday(x)
  d <- x - start + 1
  hday <- ifelse(d %% 366 == 0, 365, ((d-1) %% 365 ) + 1 )

  return(hday)
}


format.jday <- function(x, ...)
{
  nam <- names(x)
  if(is.numeric(x)) {
    if(all(x > 0 & x < 366)){
      x <- as.Date(as.numeric(x)-1, origin = "1970-01-01")
    } else {
      stop("Argument `x` must be either date or an integer inside [1, 365].")
    }
  }

  x <- as.Date(x)
  month <- month.abb[as.numeric(format(x, "%m"))]

  day <- as.numeric(format(x, "%d"))
  suffix <- .ordinal_suffix(day)
  day <- format(paste0(day, suffix), width = 4, justify = "right")

  y <- paste0(month, " ", day, " (day ",
              format(as.numeric(format(x, "%j")), width = 3, justify = "right"), ")")
  names(y) <- nam

  return(y)
}

# type_sum <- function(x)
# {
#   UseMethod("type_sum")
# }
#
# type_sum.jday <- format.jday

# as.character.jday <- format.jday

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

circular_sd <- function(x, lwr = 0, upr = 365)
  .circular_stats(x = x, lwr = lwr, upr = upr)["sd"]

# circular_cv <- function(x, lwr = 0, upr = 365){
#   y <- .circular_stats(x = x, lwr = lwr, upr = upr)
#   return(y["sd"]/y["mean"])
# }

mean_day <- function(x, lwr = 0, upr = 365)
{
  d <- circular_mean(x = x, lwr = lwr, upr = upr)
  class(d) <- c("jday" , "numeric")

  return(d)
}

print.jday <- function(x, ...)
{
  y <- format.jday(x)
  y <- ifelse(is.na(y),  y <- NA_character_, y)
  print(y)
}

# melt <- function(x, name = NA) {
#
#   x <- enframe(x, name = "id")
#   cname <- names(x$value[[1]])
#   name <- if(is.na(name) | length(name) == 0) {
#     if(!is.null(cname)) cname else "value"
#   } else {
#     name
#   }
#
#   if(!is.null(cname)) {
#     x %>%
#       mutate(parameter = purrr::map_chr(value, .f = names),
#              value = purrr::map(value, .f = unlist))
#   }
#
#   y <- rename(x, !!name := value) %>%
#     unnest
#
#   return(y)
# }


.compute_new_vars <- function(x, .vars)
{
  att <- attr_smires(x)
  if(length(.vars) == 0) stop("You need to specify 'metric.vars'. ")

  x <- mutate(x, !!!.vars)

  # mutate drops attributes
  attr_smires(x) <- att

  return(x)
}