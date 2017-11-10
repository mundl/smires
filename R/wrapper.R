# complete = TRUE and drop_na = "group" interfere.
# maybe issue a warning, that dropped years will be completed with zeros
# todo: consider setting drop_na = "group"

char_binary <- function(x, major = min(minor), minor = NA,
                    drop_na = "none", rule = "cut", threshold = 0.001,
                    fun_group = NULL, fun_minor = NULL, fun_major = NULL,
                    fun_total = NULL,
                    state = c("no-flow", "flow", NA),
                    ...,
                    varname = "variable",
                    simplify = FALSE, complete = TRUE, plot = FALSE) {

  state <- match.arg(state, several.ok = TRUE)

  if(isTRUE(complete)) {
    # guess which periods should be completed
    complete <- if(is.function(fun_group)) {
      "group"
    } else if(is.function(fun_minor)) {
      "minor"
    } else if(is.function(fun_major)) {
      "major"
    } else {
      # No aggregation function specified, setting 'complete = FALSE'.
      FALSE
    }
  }

  if(complete == FALSE) {
    complete <- "none"
  }

  grouped <- x %>%
    group_by_interval(major_interval = major, minor_interval = minor)

  spells <- grouped %>%
    # complete spells after dropping NA periods
    find_spells(rule = rule, threshold = threshold, complete = "none") %>%
    arrange(group)

  if(plot) {
    maj <- as.numeric(grouped$major)
    # when taking the log(), replace small observations (zeros) with threshold
    # grouped$discharge[grouped$discharge < threshold] <- threshold
    grouped$rescaled <- maj + (.rescale(grouped$discharge) - 0.5)

    p <- plot_groups(spells) +
      geom_line(data = grouped, aes(x = hday, y = rescaled, group = major),
                size = 0.2)

    print(p)
  }

  # computing new variables
  spells <- .compute_new_vars(spells, ..., default = "duration")

  y <- spells %>%
    drop_na_periods(period = drop_na) %>%
    .complete_spell(complete = complete, fill = 0) %>%
    arrange(group)

  if(is.function(fun_group)) {
    y <- y %>% group_by(group, minor, major, state) %>%
      do(var = fun_group(.$var)) %>%
      # only needed until unnest() can handle lists
      # https://github.com/tidyverse/tidyr/issues/278
      ungroup() %>%
      mutate_if(is.list, simplify_all) %>%
      unnest()
  }

  if(is.function(fun_minor)) {
    y <- y %>% group_by(minor, state) %>%
      do(var = fun_minor(.$var)) %>%
      # only needed until unnest() can handle lists
      # https://github.com/tidyverse/tidyr/issues/278
      ungroup() %>%
      mutate_if(is.list, simplify_all) %>%
      unnest()
  }

  if(is.function(fun_major)) {
    if(is.function(fun_minor)) {
      stop("You can eihter aggregate by minor interval or major interval, not both.")
    }
    y <- y %>% group_by(major, state) %>%
      do(var = fun_major(.$var)) %>%
      # only needed until unnest() can handle lists
      # https://github.com/tidyverse/tidyr/issues/278
      ungroup() %>%
      mutate_if(is.list, simplify_all) %>%
      unnest()
  }

  if(is.function(fun_total)) {
    y <- y %>% group_by(state) %>%
      summarize(var = fun_total(var))
  }

  y <- y %>%
    rename(!!varname := var) %>%
    filter(state %in% !!state) %>%
    ungroup()

  if(simplify){
    y <- y[[varname]]
    if(is.difftime(y)) y <- as.double(y)
    if(length(y) == 1 & varname != "variable") names(y) <- varname
  }

  if(length(y) == 0) y <- NA

  return(y)
}

smires <- function(...)
{
  warning("The usage of the function 'metric()' for the calculation of metrics from continous variables is deprecated. Please use 'char_cont()' instead.")
  char_binary(...)
}


char_cont <- function(x, major = min(minor), minor = NA,
                      drop_na = "none", threshold = 0.001,
                      fun_group = NULL, fun_minor = NULL, fun_major = NULL,
                      fun_total = NULL,
                      ...,
                      # invar = "discharge",
                      varname = "variable",
                      simplify = FALSE, plot = FALSE) {

  grouped <- x %>%
    group_by_interval(major_interval = major, minor_interval = minor)

  # computing new variables
  grouped <- .compute_new_vars(grouped, ...)

  maj <- as.numeric(grouped$major)
  grouped$rescaled <- maj + (.rescale(grouped$var) - 0.5)#*0.65

  if(plot) print(plot_groups(grouped))

  y <- grouped %>%
    drop_na_periods(period = drop_na)

  if(is.function(fun_group)) {
    y <- y %>% group_by(group, minor, major) %>%
      do(var = fun_group(.$var)) %>%
      # only needed until unnest() can handle lists
      # https://github.com/tidyverse/tidyr/issues/278
      ungroup() %>%
      mutate_if(is.list, simplify_all) %>%
      unnest()
  }

  if(is.function(fun_minor)) {
    # y <- y %>% group_by(minor) %>%
    #   do(var = fun_minor(.$var)) %>%
    #   # only needed until unnest() can handle lists
    #   ungroup() %>%
    #   mutate_if(is.list, simplify_all) %>%
    #   unnest()

    y <- y %>% group_by(minor) %>%
      do(var = fun_minor(.$var)) %>%
      filter(length(var) > 1) %>%
      unnest()
  }

  if(is.function(fun_major)) {
    if(is.function(fun_minor)) {
      stop("You can eihter aggregate by minor interval or major interval, not both.")
    }
    # y <- y %>% group_by(major) %>%
    #   do(var = fun_major(.$var)) %>%
    #   # only needed until unnest() can handle lists
    #   ungroup() %>%
    #   mutate_if(is.list, simplify_all) %>%
    #   unnest()

    y <- y %>% group_by(major) %>%
      do(var = fun_major(.$var)) %>%
      filter(length(var) >= 1) %>%
      unnest()
  }

  if(is.function(fun_total)) {
    y <- ungroup(y) %>%
      summarize(var = fun_total(var))
  }

  y <- y %>%
    rename(!!varname := var)%>%
    ungroup()

  if(simplify){
    y <- y[[varname]]
    if(is.difftime(y)) y <- as.double(y)
    if(length(y) == 1 & varname != "variable") names(y) <- varname
  }

  if(length(y) == 0) y <- NA

  return(y)
}

metric <- function(...)
{
  warning("The usage of the function 'metric()' for the calculation of metrics from continous variables is deprecated. Please use 'char_cont()' instead.")
  char_cont(...)
}
