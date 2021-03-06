.metrics <- new.env()
.metrics$list <- list()

metrics <- function(markup = TRUE)
{
  x <- do.call(rbind, .metrics$list)

  if(markup) x$Function <- paste0("`", x$Function, "(x)`")

  return(x)
}

register_metric <- function(name, description, section, acronym, fun,
                            measure, args = "")
{

  meas <- c("Location", "Variability", "Distribution")

  sec  <- c("General characteristics", "Number of no-flow days",
            "Duration", "Timing and seasonality", "Flow spell characteristics",
            "Rate of change before/after no-flow event")

  # check if functions exists
  if(!exists(x = fun, mode = "function"))
    stop("Function '", fun, "' does not exist in package 'smires'.")

  # check if functions exists
  if(acronym %in% metrics()$Acronym)
    stop("Acronym ", acronym, " is already in use.")

   current <- data_frame(Name = name,
                        Acronym = acronym,
                        Function = fun,
                        Description = description,
                        Measure = factor(measure, levels = meas),
                        section = factor(section, levels = sec),
                        args = args)

  .metrics$list[[fun]] <- current
}


# General characteristics ----
no_flow_years <- function(..., threshold = 0.001) {
  char_binary(..., threshold = threshold, rule = "cut", complete = FALSE) %>%
    mutate(year = as.numeric(major)) %>%
    select(state, year) %>%
    distinct() %>%
    summarize(f0 = sum(state == "no-flow", na.rm = TRUE) / max(year)) %>%
    unlist()
}

register_metric(
  name = "Proportion of no-flow years",
  description = "Number of years with no-flow occurrence (when flow is at least for one day below the no-flow threshold) divided by the study period in years",
  measure = NA,
  section = "General characteristics",
  acronym = "$f_0$",
  fun = "no_flow_years")


# Nuber of flow days ----
MAN <- function(..., threshold = 0.001) {
  char_binary(..., threshold = threshold, fun_major = sum, fun_total = mean,
              drop_na = "major",
              rule = "cut", state = "no-flow", varname = "MAN", simplify = TRUE)
}

register_metric(
  name = "Mean annual number of no-flow days",
  description = "Mean annual number of days with no-flow occurrence (when flow is below the no-flow threshold)",
  measure = "Location",
  section = "Number of no-flow days",
  acronym = "$MAN$",
  fun = "MAN",
  args = "fun_major = sum, fun_total = mean, drop_na = 'major', rule = 'cut'")


#
cv <- function(x) {
  x <- as.double(x)
  sd(x) / mean(x)
}

CVAN <- function(..., threshold = 0.001) {
  char_binary(..., threshold = threshold, fun_major = sum, fun_total = cv,
              drop_na = "major",
              rule = "cut", state = "no-flow", varname = "CVAN", simplify = TRUE)
}

register_metric(
  name = "CV of the annual number of no-flow days",
  description = "Coefficient of variation of the annual number of days with no-flow occurrence (when flow is below the no-flow threshold)",
  measure = "Variability",
  section = "Number of no-flow days",
  acronym = "$CVAN$",
  fun = "CVAN",
  args = "fun_major = sum, fun_total = cv, drop_na = 'major', rule = 'cut'")


#
FAN <- function(..., threshold = 0.001) {
  char_binary(..., threshold = threshold, fun_major = sum,
              drop_na = "major", rule = "cut",
              complete = TRUE, state = "no-flow", varname = "FAN", simplify = TRUE)
}

register_metric(
  name = "Distribution of the annual number of no-flow days",
  description = "Emperical distribution (ecdf) of the annual number of days with no-flow occurrence (when flow is below the no-flow threshold)",
  measure = "Distribution",
  section = "Number of no-flow days",
  acronym = "$FAN$",
  fun = "FAN",
  args = "fun_major = sum, drop_na = 'major', rule = 'cut', complete = TRUE")



# Duration -----
MAMD <- function(..., threshold = 0.001) {
  char_binary(..., threshold = threshold, fun_major = max, fun_total = mean,
              drop_na = "major",
              state = "no-flow", varname = "MAMD", simplify = TRUE)
}

register_metric(
  name = "Mean annual maximum duration",
  description = "Mean duration of the longest annual no-flow event (during which flow remains below the no-flow threshold)",
  measure = "Location",
  section = "Duration",
  acronym = "$MAMD$",
  fun = "MAMD",
  args = "fun_major = max, fun_total = mean, drop_na = 'major', rule = 'onset'")


#
CVAMD <- function(..., threshold = 0.001) {
  char_binary(..., threshold = threshold, fun_major = max, fun_total = cv,
              drop_na = "major",
              state = "no-flow", varname = "CVAMD", simplify = TRUE)
}

register_metric(
  name = "CV of annual maximum duration",
  description = "Coefficient of variation of the duration of the longest annual no-flow event (during which flow remains below the no-flow threshold)",
  measure = "Variability",
  section = "Duration",
  acronym = "$CVAMD$",
  fun = "CVAMD",
  args = "fun_major = max, fun_total = mean, drop_na = 'major', rule = 'onset'")

#
FAMD <- function(..., threshold = 0.001) {
  char_binary(..., threshold = threshold, fun_major = max,
              drop_na = "major", complete = TRUE,
              state = "no-flow", varname = "FAMD", simplify = TRUE)
}

register_metric(
  name = "Distribution of annual maximum duration",
  description = "Emperical distribution (ecdf) of the duration of the longest annual no-flow event (during which flow remains below the no-flow threshold)",
  measure = "Distribution",
  section = "Duration",
  acronym = "$FAMD$",
  fun = "FAMD",
  args = "fun_major = max, drop_na = 'major', rule = 'onset'")

# Timing and Seasonality ----

tau0 <- function(..., threshold = 0.001, format = TRUE) {
  x <- char_binary(..., threshold = threshold,
                   fun_major = min, fun_total = mean_day, drop_na = "major",
                   spell.vars = vars(jday = julian_day(onset)), state = "no-flow",
                   simplify = TRUE, complete = FALSE)
  if (format) x <- format(x)

  return(x)
}

register_metric(
  name = "Mean onset",
  description = "Mean Julian date (day-of-year) of the first annual no-flow day (using circular statistics)",
  measure = "Location",
  section = "Timing and seasonality",
  acronym = "$\\tau_0$",
  fun = "tau0",
  args = "fun_major = min, fun_total = mean_day, drop_na = 'major', spell.vars= vars(jday = julian_day(onset)), simplify = TRUE")

#
tau0r <- function(..., threshold = 0.001, format = TRUE) {
  x <- char_binary(..., threshold = threshold, fun_major = min, fun_total = circular_r, drop_na = "major",
                   spell.vars = vars(jday = julian_day(onset)), state = "no-flow",
                   simplify = TRUE, complete = FALSE)
  if (format) x <- format(x)

  return(x)
}

register_metric(
  name = "Variability of onset",
  description = "Circular variability index r (between 0 and 1) of the onset",
  measure = "Variability",
  section = "Timing and seasonality",
  acronym = "$\\tau_{0r}$",
  fun = "tau0r",
  args = "fun_major = min, fun_total = circular_r, drop_na = 'major', spell.vars= vars(jday = julian_day(onset)), simplify = TRUE, complete = FALSE")

#
tau0f <- function(..., threshold = 0.001, format = TRUE) {
  x <- char_binary(..., threshold = threshold, drop_na = "major",
                   spell.vars = vars(jday = julian_day(onset)), state = "no-flow",
                   simplify = TRUE, complete = FALSE)
  if (format) x <- format(x)

  return(x)
}

register_metric(
  name = "Empirical Distribution of onset",
  description = "Emperical distribution (ecdf) of the onset",
  measure = "Variability",
  section = "Timing and seasonality",
  acronym = "$\\tau_{0f}$",
  fun = "tau0f",
  args = " drop_na = 'major', spell.vars= vars(jday = julian_day(onset)), state = 'no-flow', simplify = TRUE, complete = FALSE")

#
tauE <- function(..., threshold = 0.001, format = TRUE) {
  x <- char_binary(..., threshold = threshold,
                   fun_major = min, fun_total = mean_day, drop_na = "major",
                   spell.vars = vars(jday = julian_day(termination)),
                   state = "no-flow", simplify = TRUE, complete = FALSE)
  if (format) x <- format(x)

  return(x)
}

register_metric(
  name = "Mean termination",
  description = "Mean Julian date (day-of-year) of the end of the first no-flow spell (using circular statistics)",
  measure = "Location",
  section = "Timing and seasonality",
  acronym = "$\\tau_E$",
  fun = "tauE",
  args = "fun_major = min, fun_total = mean_day, drop_na = 'major',spell.vars= vars(jday = julian_day(termination)),  simplify = TRUE, complete = FALSE")

#
tauEr <- function(..., threshold = 0.001, format = TRUE) {
  x <- char_binary(..., threshold = threshold, fun_major = min,
                   fun_total = circular_r, drop_na = "major",
                   spell.vars = vars(jday = julian_day(termination)),
                   state = "no-flow", simplify = TRUE, complete = FALSE)
  if (format) x <- format(x)

  return(x)
}

register_metric(
  name = "Variability of termination",
  description = "Circular variability index r (between 0 and 1) of the termination",
  measure = "Variability",
  section = "Timing and seasonality",
  acronym = "$\\tau_{Er}$",
  fun = "tauEr",
  args = "fun_major = min, fun_total = circular_r, drop_na = 'major', spell.vars= vars(jday = julian_day(termination)), simplify = TRUE, complete = FALSE")


#
tau <- function(x, threshold = 0.001, format = TRUE) {
  x <- .append_flow_state(x, threshold = threshold) %>%
    mutate(jday = julian_day(time)) %>%
    filter(state == "no-flow")

  x <- if (nrow(x) == 0) {
    NA
  } else {x %>%
      summarize(variable = mean_day(jday)) %>%
      unlist(use.names = FALSE)
  }

  if (format) x <- format(x)

  return(x)
}

register_metric(
  name = "Mean seasonality",
  description = "Mean Julian date of all no-flow days (using circular statistics)",
  measure = "Location",
  section = "Timing and seasonality",
  acronym = "$\\tau$",
  fun = "tau")

#
taur <- function(x, threshold = 0.001, format = TRUE) {
  x <- .append_flow_state(x, threshold = threshold) %>%
    mutate(jday = julian_day(time)) %>%
    filter(state == "no-flow")

  x <- if (nrow(x) == 0) {
    NA
  } else {x %>%
    summarize(variable = circular_r(jday)) %>%
    unlist(use.names = FALSE)
  }

  if (format) x <- format(x)

  return(x)
}

register_metric(
  name = "Strength of seasonality",
  description = "Circular variability index r (between 0 and 1) of all no-flow days",
  measure = "Variability",
  section = "Timing and seasonality",
  acronym = "$\\tau_r$",
  fun = "taur")


# Rate of change ----
k <- function(...) {
  char_cont(..., fun_major = recession,
            fun_total = function(x) mean(x, na.rm = TRUE),
            drop_na = "major", simplify = TRUE)
}

register_metric(
  name = "Mean recession rate",
  description = "Mean rate of decay of the hydrograph during flow recession periods",
  measure = "Location",
  section = "Rate of change before/after no-flow event",
  acronym = "$k$",
  fun = "k")

#
ksd <- function(...) {
  char_cont(..., fun_major = recession, fun_total = sd, drop_na = "major",
            simplify = TRUE)
}

register_metric(
  name = "Standard deviation of recession rate",
  description = "Standard deviation of rate of decay of the hydrograph during flow recession periods",
  measure = "Variability",
  section = "Rate of change before/after no-flow event",
  acronym = "$k_{sd}$",
  fun = "ksd")



is_turning_point <- function(x)
{
  delta <- sign(c(0, diff(x)))

  # fill zeros with last observation
  nonzero <- delta != 0
  d1 <- c(NA, delta[nonzero])[cumsum(nonzero) + 1]

  pairs <- embed(d1, 2)[, 2:1]
  isTP <- (pairs[, 1] != pairs[, 2])

  tp <- logical(length(x))
  tp[isTP] <- TRUE

  return(tp)
}

is_reversal <- function(x)
{
  tp <- is_turning_point(x)

  # mask every second turning point
  mask <- which(tp)[c(TRUE, FALSE)]
  tp[mask] <- FALSE

  return(tp)
}


# this is equivalent to: number of flood peaks or number of low flows periods
nrv <- function(...) {
  char_cont(..., fun_major = sum, drop_na = "major", fun_total = mean,
            metric.vars = vars(tp = is_reversal(discharge)), simplify = TRUE)
}

register_metric(
  name = "Mean annual number of reversals in flow magnitude",
  description = "Mean annual number of reversals in flow magnitude",
  measure = "Location",
  section = "Rate of change before/after no-flow event",
  acronym = "$n_{rv}$",
  fun = "nrv")


# number of turning points
# this is equivalent to: number of flood peaks or number of low flows periods
# see
# .detect_increase(balder) %>%
#   .add_spellvars()

# ntp <- function(...) {
#   char_cont(..., fun_major = sum, drop_na = "major", fun_total = mean,
#          tp = is_turning_point(discharge), simplify = TRUE)
# }




# rewettening metrics

which_max <- function(x)
{
  i <- which.max(x)
  if (length(i) == 0) i <- NA
  return(i)
}

# char_binary(balder, major = 1,
#             spell.vars = vars(max = max(discharge, na.rm = TRUE),
#                              dt = which_max(discharge)),
#             state = "flow")
# #
# char_binary(balder, major = 1,
#             spell.vars = vars(max = max(discharge, na.rm = TRUE),
#                              Qt5 = discharge[5]),
#             state = "flow")
