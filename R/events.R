find_spells <- function(x, threshold = 0.001,
                        rule = c("cut", "duplicate", "start", "end"),
                        na.rm = TRUE, warn = TRUE)
{
  rule <- match.arg(rule)

  x %>%
    .detect_noflow_spells(threshold = threshold) %>%

    .add_spellvars(warn = warn, duplicate = rule != "cut") %>%
    assign_spell(rule = rule) %>%
    arrange(spell) # sort by spell
}


assign_spell <- function(x, rule = c("cut", "duplicate", "start", "end"))
{
  rule <- match.arg(rule)
  x <- set_attr_smires(x, "rule", rule)

  # todo: rules for "majority" and "center"

  # spells are already cut or duplicated
  if(rule %in% c("cut", "duplicate")) return(x)

  if(rule == "start") {
    y <- arrange(ungroup(x), spell, group) %>%
      distinct(spell, .keep_all = TRUE)
  }

  if(rule == "end") {
    y <- arrange(ungroup(x), desc(spell), desc(group)) %>%
      distinct(spell, .keep_all = TRUE) %>%
      arrange(spell)
  }

  return(y)
}


.detect_noflow_spells <- function(x, threshold = 0.1)
{
  if(is.null(threshold))
  {
    x$spell <- seq_len(nrow(x))
  } else {
    att <- get_attr_smires(x)

    x$state <- ifelse(x$discharge <= threshold, "no-flow", "flow")
    x$state <- factor(x$state, levels = c("no-flow", "flow"))
    x <- mutate(x, spell = .spell(x$state))

    att[["threshold"]] <- threshold
    x <- set_attr_smires(x, value = att)
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

  att <- get_attr_smires(x)

  # always store cutted spells in attributes,  needed for plotting
  if(grouped) {
    cut <- group_by(x, spell, state, group)
  } else{
    cut <- group_by(x, spell, state)
  }
  cut <- cut %>%
    summarize(start = min(time), end = max(time) + att$dt,
              duration = end - start)

  if(duplicate) {
    res <- y %>% do(data.frame(start = min(.$time), end = max(.$time) + att$dt,
                               group = unique(.$group))) %>%
      mutate(duration = end - start)
  } else {
    res <- summarize(y, start = min(time), end = max(time) + att$dt,
                     duration = end - start)
  }

  if(grouped) {
    # merge with minor an major intervals, if data was grouped
    res <- right_join(res, att[["group_interval"]], by = "group")
    cut <- right_join(cut, att[["group_interval"]], by = "group")
  }

  # quick and dirty way to drop smires attributes, no need to store them twice
  att[["spell_cut"]] <- cut[, seq_along(cut)]

  #if(grouped | duplicate)
  res <- set_attr_smires(res, value = att)


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

