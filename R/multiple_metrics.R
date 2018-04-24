
multiple_metrics <- function(x, .funs, threshold = 0.001, parallel = FALSE, ...)
{

  # make sure return values are eihter length 1 or lists
  .funs <- lapply(.funs, .list_if_long)

  # multidplyr cannot handle rowwise()
  # don't know how to map vars() und funs() at the same time
  if (parallel) parallel <- FALSE

  y <- x %>%
    #    mutate(.rownumber = row_number()) %>%
    rowwise()
  #group_by(.rownumber)

  # if (parallel) {
  #   library(multidplyr)
  #   y <- partition(y)
  #
  #   cluster_library(y, c("purrr", "smires"))
  # }


  y <- y %>%
    mutate_at(.funs = .funs, .vars = vars(data), threshold = threshold, ...) %>%
    ungroup()

  # if (parallel) {
  #     y <- collect(y) %>%
  #     ungroup() %>%
  #     select(-PARTITION_ID) %>%
  #     arrange(.rownumber)
  # }
  #
  #   select(y, -.rownumber) %>%
  #     ungroup()
}

all_metrics <- function(x, threshold = 0.001, format = FALSE,
                        parallel = FALSE, ...)
{
  # get all smires functions
  fnames <- metrics(markup = FALSE)$Function
  f <- lapply(fnames, getFunction, where = "package:smires")
  names(f) <- fnames

  multiple_metrics(x, .funs = f, threshold = threshold, parallel = parallel, ...)
}


.list_if_long <- function(.f)
{
  function(...) {
    r <- .f(...)
    if (length(r) > 1) r <- list(r)
    return(r)
  }
}


.test_metrics <- function(x, funs = NULL, threshold = 0.001, format = FALSE, ...)
{

  if (is.null(funs)) {
    # get all smires functions
    fnames <- metrics(markup = FALSE)$Function
    funs <- lapply(fnames, getFunction, where = "package:smires")
    names(funs) <- fnames
  } else {
    fnames <- names(funs)
    call <- deparse(substitute(funs))
    call <- strsplit(gsub("^[^(]+\\(|\\)$", "", call), split = ", ")[[1]]
    fnames <- ifelse(fnames == "", call, fnames)
  }

  # and make sure return values are eihter length 1 or lists
  funs <- lapply(funs, .list_if_long)

  problems <- data_frame(data = numeric(), fun = character(),
                         error = character())
  for (row in seq_along(x$data)) {
    for (col in seq_along(funs)) {
      safefun <- safely((funs[[col]]), otherwise = NULL)

      res <- safefun(x$data[[row]], threshold = threshold, ...)
      if (!is.null(res$error)) {
        problems <- bind_rows(data_frame(data = row,
                                         fun = fnames[col],
                                         error = res$error$message))
      }
    }
  }

  if (nrow(problems))   return(problems)
}


# smires %>%
#   .test_metrics()

