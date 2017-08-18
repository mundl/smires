find_spells <- function(x, threshold = 0.001, rule = "cut", na.rm = TRUE,
                        warn = TRUE, complete = "none")
{
  x %>%
    .append_flow_state(threshold = threshold) %>%
    summarize_spell(rule = rule, na.rm = nr.rm, warn = warn,
                    complete = complete)
}

summarize_spell <- function(x,
                            rule = c("cut", "duplicate", "onset", "termination"),
                            na.rm = TRUE, warn = TRUE, complete = "none")
{
  rule <- match.arg(rule)

  x %>%
    .add_spellvars(warn = warn, duplicate = rule != "cut") %>%
    .assign_spell(rule = rule) %>%
    .complete_spell(complete = complete) %>%
    arrange(spell) # sort by spell
}



.assign_spell <- function(x,
                          rule = c("cut", "duplicate", "onset", "termination"))
{
  rule <- match.arg(rule)
  x <- .set_attr_smires(x, "rule", rule)

  # todo: rules for "majority" and "center"
  # todo: cut = cut_group = cut_minor, cut_major

  # spells are already cut or duplicated
  if(rule %in% c("cut", "duplicate")) {
    y <- x
  }

  if(rule == "onset") {
    y <- arrange(ungroup(x), spell, group) %>%
      distinct(spell, state, .keep_all = TRUE)
  }

  if(rule == "termination") {
    y <- arrange(ungroup(x), desc(spell), desc(group)) %>%
      distinct(spell, state, .keep_all = TRUE) %>%
      arrange(spell)
  }

  return(y)
}

.complete_spell <- function(x, complete = c("none", "major", "minor", "group"),
                            fill = NA)
{
  complete <- match.arg(complete)

  # retain zero length events
  fill <- list(onset = NA, termination = NA, duration = 0,
               group = NA, major = NA, minor = NA, var = fill)


  x <- ungroup(x)
  y <- switch(complete,
              major = complete(x, state, major, fill = fill),
              minor = complete(x, state, minor, fill = fill),
              group = complete(x, state, group, fill = fill),
              x)

  return(y)
}


.append_flow_state <- function(x, threshold = 0.001)
{
  if(is.null(threshold))
  {
    x$spell <- seq_len(nrow(x))
  } else {
    att <- .get_attr_smires(x)

    # todo, better use cut?
    # cut(, breaks = c(0, threshold, Inf), labels = c("no-flow", "flow"))
    x$state <- ifelse(x$discharge <= threshold, "no-flow", "flow")
    x$state <- factor(x$state, levels = c("no-flow", "flow"))
    x <- mutate(x, spell = .spell(x$state))

    att[["threshold"]] <- threshold
    x <- .set_attr_smires(x, value = att)
  }


  return(x)
}



.add_spellvars <- function(x, warn = TRUE, duplicate = FALSE)
{
  grouped <- "group" %in% colnames(x)

  y <- if(grouped && !duplicate) {
    group_by(x, spell, state, group)
  } else {
    group_by(x, spell, state)
  }

  att <- .get_attr_smires(x)

  # always store cutted spells in attributes,  needed for plotting
  if(grouped) {
    cut <- group_by(x, spell, state, group)
  } else{
    cut <- group_by(x, spell, state)
  }
  cut <- cut %>%
    summarize(onset = min(time), termination = max(time) + att$dt,
              duration = termination - onset)


  if(duplicate) {
    res <- y %>% do(data.frame(onset = min(.$time), termination = max(.$time) + att$dt,
                               group = unique(.$group))) %>%
      mutate(duration = termination - onset)
  } else {
    res <- summarize(y, onset = min(time), termination = max(time) + att$dt,
                     duration = termination - onset)
  }


  if(grouped) {
    # merge with minor an major intervals, if data was grouped
    res <- right_join(res, att[["group_interval"]], by = "group")
    cut <- right_join(cut, att[["group_interval"]], by = "group")
  }

  # quick and dirty way to drop smires attributes, no need to store them twice
  att[["spell_cut"]] <- cut[, seq_along(cut)]

  #if(grouped | duplicate)
  res <- .set_attr_smires(res, value = att)


  return(res)
}


.spell <- function(x, new.group.na = TRUE, as.factor = TRUE)
{
  # copied from lfstat group()
  # operates just on the grouping variable

  x <- as.numeric(as.factor(x))

  if(!new.group.na) {
    s <- seq_along(x)
    finite <- !is.na(x)
    x <- approx(s[finite], x[finite], xout = s, f = 0,
                method = "constant", rule = c(1, 2))$y
  } else {
    # treat NAs as a group of its own
    # there isn't yet a level zero, therefore NAs can become zeros
    x[is.na(x)] <- 0
  }

  inc <- diff(x)
  if (new.group.na) inc[is.na(inc)] <- Inf

  grp <- c(0, cumsum(inc != 0))

  if(grp[1] == 0) grp <- grp + 1

  if(as.factor) {
    return(factor(grp, ordered = TRUE))
  } else {
    return(grp)
  }
}

