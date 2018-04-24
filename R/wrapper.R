# complete = TRUE and drop_na = "group" interfere.
# maybe issue a warning, that dropped years will be completed with zeros
# todo: consider setting drop_na = "group"

char_binary <- function(x, major = min(minor), minor = NA,
                        drop_na = "none", rule = "cut", threshold = 0.001,
                        fun_group = NULL, fun_minor = NULL, fun_major = NULL,
                        fun_total = NULL,
                        spell.vars = vars(duration),
                        metric.vars = NULL,
                        complete = TRUE, plot = FALSE,
                        varname = NULL,
                        simplify = FALSE,
                        state = c("no-flow", "flow", NA)) {

  spell.vars <- rlang::quos_auto_name(spell.vars)
  if (is.null(metric.vars)) metric.vars <- .guess_metric_var(spell.vars)


  state <- match.arg(state, several.ok = TRUE)

  if (isTRUE(complete)) {
    # guess which periods should be completed
    complete <- if (is.function(fun_group)) {
      "group"
    } else if (is.function(fun_minor)) {
      "minor"
    } else if (is.function(fun_major)) {
      "major"
    } else {
      # No aggregation function specified, setting 'complete = FALSE'.
      FALSE
    }
  }

  if (complete == FALSE) {
    complete <- "none"
  }

  grouped <- x %>%
    group_by_interval(major_interval = major, minor_interval = minor)

  # split variables derived from continous time series and spells
  # variables <- quos(...)
  # mask <- grepl("time|discharge", variables)
  # varCont <- variables[mask]
  # varSpell <- variables[!mask]

  spells <- grouped %>%
    # complete spells after dropping NA periods
    find_spells(rule = rule, threshold = threshold, complete = "none",
                spell.vars = spell.vars) %>%
    arrange(group)

  if (plot) {
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
  spells <- .compute_new_vars(spells, .vars = metric.vars)

  spells <- spells %>%
    drop_na_periods(period = drop_na) %>%
    .complete_spell(complete = complete, fill = 0) %>%
    arrange(group)

  y <- .char_common(data = spells,
                    fun_group = fun_group, metric.vars = metric.vars,
                    fun_minor = fun_minor, fun_major = fun_major,
                    fun_total = fun_total, state = state, simplify = simplify,
                    varname = varname)

  return(y)
}

char_cont <- function(x, major = min(minor), minor = NA,
                      drop_na = "none", threshold = 0.001,
                      fun_group = NULL, fun_minor = NULL, fun_major = NULL,
                      fun_total = NULL,
                      metric.vars = vars(discharge),
                      varname = NULL,
                      simplify = FALSE, plot = FALSE) {

  metric.vars <- rlang::quos_auto_name(metric.vars)

  grouped <- x %>%
    group_by_interval(major_interval = major, minor_interval = minor)

  # computing new variables
  grouped <- .compute_new_vars(grouped, .vars = metric.vars) %>%
    drop_na_periods(period = drop_na) %>%
    arrange(group)

  # after we calculating the new variables, use new names
  metric.vars <- .guess_metric_var(metric.vars)

  if (plot) {
    maj <- as.numeric(grouped$major)

    metric.vars <- .guess_metric_var(metric.vars)

    mvar <- pull(select_at(.tbl = grouped, metric.vars)[, 1])
    p <- grouped %>%
      mutate(rescaled = maj + (.rescale(mvar) - 0.5))#*0.65

    print(plot_groups(p))
  }

  # hack because we expect to have a column named state... :(
  grouped$state <- NA

  y <- .char_common(data = grouped,
                    fun_group = fun_group, metric.vars = metric.vars,
                    fun_minor = fun_minor, fun_major = fun_major,
                    fun_total = fun_total, state = NULL, simplify = simplify,
                    varname = varname)

  if (length(y) == 1 & !is.null(varname)) names(y) <- varname
  if (length(y) == 0) y <- NA

  return(y)
}

.char_common <- function(data, fun_group, metric.vars,
                         fun_minor, fun_major, fun_total, state = NULL,
                         simplify, varname)
{

  .robustify_function <- function(x) {

    if (is.null(x)) return(NULL)

    .f <- purrr::as_mapper(x)
    function(...) {
      r <- .f(...)
      if (length(r) == 0) r <- NULL
      r <- if (is.list(r)) r else list(r)

    }
  }

  fun_group <- .robustify_function(fun_group)
  fun_minor <- .robustify_function(fun_minor)
  fun_major <- .robustify_function(fun_major)
  fun_total <- .robustify_function(fun_total)

  unnest_jday <- function(x)
    {
    if (nrow(x) == 0) return(x)

    isList <- logical()
    for (i in seq_len(ncol(x))) isList[i] <- inherits(pull(x[, i]), "list")
    listcols <- as.list(x[, isList])

    if (sum(isList) == 0) return(x)

    isjd <- logical(length(isList))
    isjday <- sapply(listcols, function(x) all(sapply(x, inherits, "jday")))
    isjd[which(isList)[isjday]] <- TRUE

    if (sum(isjd)) {
      for (i in which(isjd)) {
        vec <- unlist(x[, i], use.names = FALSE)
        class(vec) <- c("jday" , "numeric")
        x[, i] <- vec
      }
    }

    if (any(isList & !isjd)) x <- unnest(x)

    return(x)
  }


  y <- data

  if (is.function(fun_group)) {
    y <- y %>% group_by(group, minor, major, state) %>%
      summarize_at(.vars = metric.vars, .funs = funs(fun_group)) %>%
      filter_at(.vars = metric.vars, .vars_predicate = all_vars(lengths(.) > 0)) %>%
      unnest_jday()
  }

  if (is.function(fun_minor)) {
    y <- y %>% group_by(minor, state) %>%
      summarize_at(.vars = metric.vars, .funs = funs(fun_minor))  %>%
      filter_at(.vars = metric.vars, .vars_predicate = all_vars(lengths(.) > 0)) %>%
      unnest_jday()
  }

  if (is.function(fun_major)) {
    if (is.function(fun_minor)) {
      stop("You can eihter aggregate by minor interval or major interval, not both.")
    }
    y <- y %>% group_by(major, state) %>%
      summarize_at(.vars = metric.vars, .funs = funs(fun_major)) %>%
      filter_at(.vars = metric.vars, .vars_predicate = all_vars(lengths(.) > 0)) %>%
      unnest_jday()
  }

  if (is.function(fun_total)) {
    y <- y %>% group_by(state) %>%
      summarize_at(.vars = metric.vars, .funs = funs(fun_total))  %>%
      filter_at(.vars = metric.vars, .vars_predicate = all_vars(lengths(.) > 0)) %>%
      unnest_jday()
  }

  if (!is.null(state)) {
    y <- y %>%
      filter(state %in% !!state) %>%
      ungroup()
  }

  # hack to get rid of NA states for continous variables
  if (all(is.na(y$state))) y <- select(y, -state)


  if (simplify) y <- .simplify_metric(x = y, metric.vars = metric.vars)

  if (length(y) == 1 & !is.null(varname)) names(y) <- varname
  if (length(y) == 0) y <- NA

  return(y)
}


.guess_metric_var <- function(x)
{

  for (i in seq_along(x)) {
    na <- names(x)[i]
    x[[i]] <- rlang::quo_set_expr(x[[i]], rlang::sym(na))
  }

  return(x)
}


.simplify_metric <- function(x, metric.vars)
{

  if (length(metric.vars) != 1) stop("Can only simplify a result with a single 'metric.var'.")

  y <- ungroup(x) %>%
    select_at(metric.vars)

  value <- pull(y)
  if (is.difftime(value)) value <- as.double(value, units = "days")


  # name the vector after the variable when of length one
  if (length(value) == 1) {
    names(value) <- colnames(y)
  } else {
    # use only varying variables for names
    rest <- x[, setdiff(names(x), names(y))]
    if (ncol(rest) == 1) {
      # trivial, just one additional variable
      names(value) <- rest[, 1]
    } else{
      # remove columns which are not varying
      same <- sapply(rest, function(x) length(unique(x)) == 1)
      name <- do.call(paste, c(list(sep = "."), rest[, !same, drop = FALSE]))
      names(value) <- name
    }
  }


  return(value)
}

metric <- function(...)
{
  warning("The usage of the function 'metric()' for the calculation of metrics from continous variables is deprecated. Please use 'char_cont()' instead.")
  char_cont(...)
}

smires <- function(...)
{
  warning("The usage of the function 'smires()' for the calculation of metrics from binary variables is deprecated. Please use 'char_binary()' instead.")
  char_binary(...)
}
