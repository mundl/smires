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

.factor_fullseq <- function(x, prefix = "", ordered = TRUE)
{
  fseq <- full_seq(x, 1)

  factor(x, levels = fseq, labels = paste0(prefix, fseq), ordered = ordered)
}


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