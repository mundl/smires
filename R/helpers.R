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

