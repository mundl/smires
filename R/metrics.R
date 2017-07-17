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
         rule = "cut", state = "no-flow", outvar = "MAN", simplify = TRUE)
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
         rule = "cut", state = "no-flow", outvar = "CVAN", simplify = TRUE)
}


#
tbl_metric(name = "Distribution of the annual number of no-flow days",
           description = "Statistical distribution (cdf) of the annual number of days with no-flow occurrence (when flow is below the no-flow threshold)",
           section = "Number of no-flow days",
           acronym = "FAN",
           fun = "FAN",
           args = "fun_major = sum, drop_na = 'major', rule = 'cut', complete = TRUE")
# todo
FAN <- function(...) {
  smires(..., fun_major = sum, drop_na = "major", rule = "cut",
         complete = TRUE, state = "no-flow", outvar = "FAN", simplify = TRUE)
}

smires(balder, fun_major = sum, drop_na = "major", rule = "cut",
       complete = "major", state = "no-flow", outvar = "FAN", simplify = FALSE)


# Duration -----
tbl_metric(name = "Mean annual maximum duration",
           description = "Mean duration of the longest annual no-flow event (during which flow remains below the no-flow threshold)",
           section = "Duration",
           acronym = "MAMD",
           fun = "MAMD",
           args = "fun_major = max, fun_total = mean, drop_na = 'major', rule = 'onset'")

MAMD <- function(...) {
  smires(..., fun_major = max, fun_total = mean, drop_na = "major",
         rule = "onset", state = "no-flow", outvar = "MAMD", simplify = TRUE)
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
         rule = "onset", state = "no-flow", outvar = "CVAMD", simplify = TRUE)
}

#
tbl_metric(name = "Distribution of annual maximum duration",
           description = "Statistical distribution (cdf) of the duration of the longest annual no-flow event (during which flow remains below the no-flow threshold)",
           section = "Duration",
           acronym = "FAMD",
           fun = "FAMD",
           args = "fun_major = max, drop_na = 'major', rule = 'onset'")

FAMD <- function(...) {
  smires(..., fun_major = max, drop_na = "major", drop = FALSE,
         rule = "onset", state = "no-flow", outvar = "FAMD", simplify = TRUE)
}


# smires(ampneyBrook, fun_major = max, drop_na = "major", complete = FALSE,
#        rule = "onset", state = "no-flow", outvar = "FAMD", simplify = TRUE)
