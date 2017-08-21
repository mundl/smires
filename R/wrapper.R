smires <- function(x, major = min(minor), minor = intervals$month,
                   drop_na = "none", rule = "onset", threshold = 0.001,
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

  spells <- x %>%
    group_by_interval(major_interval = major, minor_interval = minor) %>%
    # complete spells after dropping NA periods
    find_spells(rule = rule, threshold = threshold, complete = "none") %>%
    arrange(group)

  if(plot) print(plot_groups(spells))

  # computing new variables
  variables <- quos(...)
  if(length(variables)) {
    spells <- mutate(spells, !!!variables) %>%
      rename(var :=!!names(variables)[[1]])
  } else {
    spells[, "var"] <- spells[, "duration"]
  }

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

  return(y)
}
metric <- function(x, major = min(minor), minor = intervals$month,
                   drop_na = "group", threshold = 0.001,
                   fun_group = NULL, fun_minor = NULL, fun_major = NULL,
                   fun_total = NULL,
                   ...,
                   # invar = "discharge",
                   varname = "variable",
                   simplify = FALSE, plot = FALSE) {

  grouped <- x %>%
    group_by_interval(major_interval = major, minor_interval = minor)

  # computing new variables
  variables <- quos(...)
  if(length(variables)) {
    grouped <- mutate(grouped, !!!variables) %>%
      rename(var :=!!names(variables)[[1]])
  } else {
    grouped[, "var"] <- grouped[, "discharge"]
  }


  # mutate drops arguments
  maj <- as.numeric(grouped$major)
  grouped$rescaled <- maj + (.rescale(grouped$var) - 0.4)*0.65

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
    y <- y %>% group_by(minor) %>%
      do(var = fun_minor(.$var)) %>%
      # only needed until unnest() can handle lists
      ungroup() %>%
      mutate_if(is.list, simplify_all) %>%
      unnest()
  }

  if(is.function(fun_major)) {
    if(is.function(fun_minor)) {
      stop("You can eihter aggregate by minor interval or major interval, not both.")
    }
    y <- y %>% group_by(major) %>%
     do(var = fun_major(.$var)) %>%
      # only needed until unnest() can handle lists
      ungroup() %>%
      mutate_if(is.list, simplify_all) %>%
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

  return(y)
}

