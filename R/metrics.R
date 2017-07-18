tbl_metric <- function(name = "", description = "", section = "", acronym = "",
                       fun = list(NULL),
                       args = "") {

  sec  <- c("General characteristics", "Number of no-flow days",
            "Duration", "Timing and Seasonality", "Flow spell characteristics",
            "Rate of change before/after no-flow event")

  data_frame(section = factor(section, levels = sec),
             name = name,
             description = description,
             acronym = acronym,
             fun = fun,
             args = args)
}


# General characteristics ----
tbl_metric(name = "Proportion of no-flow years",
           description = "Number of years with no-flow occurrence (when flow is at least for one day below the no-flow threshold) divided by the study period in years",
           section = "General characteristics",
           acronym = "f_0",
           fun = "no_flow_years")

no_flow_years <- function(...) {
  smires(...) %>%
    mutate(year = as.numeric(major)) %>%
    select(state, year) %>%
    distinct() %>%
    summarize(f0 = sum(state == "no-flow", na.rm = TRUE) / max(year)) %>%
    unlist()
}


# Nuber of flow days ----
tbl_metric(name = "Mean annual number of no-flow days",
           description = "Mean annual number of days with no-flow occurrence (when flow is below the no-flow threshold)",
           section = "Number of no-flow days",
           acronym = "MAN",
           fun = "MAN",
           args = "fun_major = sum, fun_total = mean, drop_na = 'major', rule = 'cut'")

MAN <- function(...) {
  smires(..., fun_major = sum, fun_total = mean, drop_na = "major",
         rule = "cut", state = "no-flow", varname = "MAN", simplify = TRUE)
}



#
tbl_metric(name = "CV of the annual number of no-flow days",
           description = "Coefficient of variation of the annual number of days with no-flow occurrence (when flow is below the no-flow threshold)",
           section = "Number of no-flow days",
           acronym = "CVAN",
           fun = "CVAN",
           args = "fun_major = sum, fun_total = cv, drop_na = 'major', rule = 'cut'")

cv <- function(x) {
  x <- as.double(x)
  sd(x) / mean(x)
}

CVAN <- function(...) {
  smires(..., fun_major = sum, fun_total = cv, drop_na = "major",
         rule = "cut", state = "no-flow", varname = "CVAN", simplify = TRUE)
}


#
tbl_metric(name = "Distribution of the annual number of no-flow days",
           description = "Statistical distribution (cdf) of the annual number of days with no-flow occurrence (when flow is below the no-flow threshold)",
           section = "Number of no-flow days",
           acronym = "FAN",
           fun = "FAN",
           args = "fun_major = sum, drop_na = 'major', rule = 'cut', complete = TRUE")

FAN <- function(...) {
  smires(..., fun_major = sum, drop_na = "major", rule = "cut",
         complete = TRUE, state = "no-flow", varname = "FAN", simplify = TRUE)
}


# Duration -----
tbl_metric(name = "Mean annual maximum duration",
           description = "Mean duration of the longest annual no-flow event (during which flow remains below the no-flow threshold)",
           section = "Duration",
           acronym = "MAMD",
           fun = "MAMD",
           args = "fun_major = max, fun_total = mean, drop_na = 'major', rule = 'onset'")

MAMD <- function(...) {
  smires(..., fun_major = max, fun_total = mean, drop_na = "major",
         state = "no-flow", varname = "MAMD", simplify = TRUE)
}

#
tbl_metric(name = "CV of annual maximum duration",
           description = "Coefficient of variation of the duration of the longest annual no-flow event (during which flow remains below the no-flow threshold)",
           section = "Duration",
           acronym = "CVAMD",
           fun = "CVAMD",
           args = "fun_major = max, fun_total = mean, drop_na = 'major', rule = 'onset'")

CVAMD <- function(...) {
  smires(..., fun_major = max, fun_total = cv, drop_na = "major",
         state = "no-flow", varname = "CVAMD", simplify = TRUE)
}

#
tbl_metric(name = "Distribution of annual maximum duration",
           description = "Statistical distribution (cdf) of the duration of the longest annual no-flow event (during which flow remains below the no-flow threshold)",
           section = "Duration",
           acronym = "FAMD",
           fun = "FAMD",
           args = "fun_major = max, drop_na = 'major', rule = 'onset'")

FAMD <- function(...) {
  smires(..., fun_major = max, drop_na = "major", complete = TRUE,
         state = "no-flow", varname = "FAMD", simplify = TRUE)
}


# Timing and Seasonality ----
tbl_metric(name = "Mean onset",
           description = "Mean Julian date (day-of-year) of the first annual no-flow day (using circular statistics)",
           section = "Timing and Seasonality",
           acronym = "tau0",
           fun = "Tau0",
           args = "fun_major = min, fun_total = mean_day, drop_na = 'major', jday = julian_day(onset)")

Tau0 <- function(...) {
  smires(..., fun_major = min, fun_total = mean_day, drop_na = "major",
         jday = julian_day(onset), state = "no-flow", simplify = TRUE)
}

tbl_metric(name = "Variability of onset",
           description = "Circular variability index r (between 0 and 1) of the onset",
           section = "Timing and Seasonality",
           acronym = "tau0r",
           fun = "Tau0r",
           args = "fun_major = min, fun_total = circular_r, drop_na = 'major', jday = julian_day(onset)")

Tau0r <- function(...) {
  smires(..., fun_major = min, fun_total = circular_r, drop_na = "major",
         jday = julian_day(onset), state = "no-flow", simplify = TRUE)
}

tbl_metric(name = "Mean termination",
           description = "Mean Julian date (day-of-year) of the first annual no-flow day (using circular statistics)",
           section = "Timing and Seasonality",
           acronym = "tau0",
           fun = "Tau0",
           args = "fun_major = min, fun_total = mean_day, drop_na = 'major', jday = julian_day(termination)")

TauE <- function(...) {
  smires(..., fun_major = min, fun_total = mean_day, drop_na = "major",
         jday = julian_day(termination), state = "no-flow", simplify = TRUE)
}

tbl_metric(name = "Variability of termination",
           description = "Circular variability index r (between 0 and 1) of the termination",
           section = "Timing and Seasonality",
           acronym = "tau0r",
           fun = "Tau0r",
           args = "fun_major = min, fun_total = circular_r, drop_na = 'major', jday = julian_day(termination)")

TauEr <- function(...) {
  smires(..., fun_major = min, fun_total = circular_r, drop_na = "major",
         jday = julian_day(termination), state = "no-flow", simplify = TRUE)
}


tbl_metric(name = "Mean seasonality",
           description = "Mean Julian date of all no-flow days (using circular statistics)",
           section = "Timing and Seasonality",
           acronym = "tau",
           fun = "Tau")

Tau <- function(x) {
  .detect_noflow_spells(x) %>%
    mutate(jday = julian_day(time)) %>%
    filter(state == "no-flow") %>%
    summarize(variable = mean_day(jday)) %>%
    unlist(use.names = FALSE)
}

tbl_metric(name = "Strength of seasonality",
           description = "Circular variability index r (between 0 and 1) of all no-flow days",
           section = "Timing and Seasonality",
           acronym = "taur",
           fun = "Taur")

Taur <- function(x) {
  .detect_noflow_spells(x) %>%
    mutate(jday = julian_day(time)) %>%
    filter(state == "no-flow") %>%
    summarize(variable = circular_r(jday)) %>%
    unlist(use.names = FALSE)
}
