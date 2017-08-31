read.metadata <- function(file, as.is = TRUE, header = TRUE, warn = TRUE, ...)
{
  tbl <- read.table(file, as.is = as.is, header = header, ...)

  if(!"filename" %in% colnames(tbl)) {
    if(warn) warning("There is no column called 'filename' in '",
                     basename(file), "'. ")
    tbl <- NA
  } else {
    invalidFilename <- is.na(tbl$filename) | tbl$filename == ""
    if(any(invalidFilename)) {
      tbl <- tbl[!invalidFilename, ]
      if(warn) warning("Dropping ", sum(invalidFilename),
                       " rows with invalid file names from '",
                       basename(file), "'.", call. = FALSE)
    }

  }

  # todo: probably check for valid colnames
  # cnames <- c("country", "station", "id", "river", "x", "y", "z", "catchment")

  return(tbl)
}

read.smires <- function(file,
                        sep = ",", dec = ".",
                        col.names,
                        timecols = "time",
                        format = "%Y-%m-%d",
                        comment.char = "#", header = TRUE,
                        metadata = NA, ...)
{

  if(length(file) > 1) {
    l <- lapply(file, FUN = read.smires, sep = sep, dec = dec,
                col.names = col.names, timecols = timecols,
                format = format,
                comment.char = comment.char, header = header,
                metadata = metadata, ...)

    att <- attr_smires(l)
    names(l) <- att$filename

    return(l)
  }

  if(missing(col.names) & !header) {
    col.names <- c("time", "discharge")
    #colClasses <- c(time = "character", discharge = "numeric")
  }

  con <- file(file)
  open(con)

  att <- list(filename = basename(file),
              dirname = dirname(file))

  if(is.data.frame(metadata)) {
    # look for current file in table of meta data
    idx <- match(basename(file), basename(metadata$filename))

    if(is.finite(idx)){
      att[names(metadata[idx, ])] <- metadata[idx, ]

    } else {
      warning("No entry for discharge file '", basename(file),
              "' in the table of meta data found.",
              "\nTrying to fetch meta data information from header of '",
              basename(file), "'", call. = FALSE)
      metadata <- NA
    }

  } else {
    if(is.list(metadata)) {
      # todo: probably check for names
      att[names(metadata)] <- metadata
    }
  }

  if(length(metadata) == 1 && is.na(metadata)) {
    # commented lines at the beginning of a file are assumend to contain
    # station meta data. read into a separate table line by line
    mheader <- list()
    repeat {
      oneLine <- readLines(con, n = 1, warn = FALSE)
      if(!substr(oneLine, 1L, 1L) == comment.char) break

      mheader <- c(mheader, oneLine)
    }

    if(length(mheader)) {
      mheader <- sapply(mheader, substr, start = 2L, stop = 1000000L)
      mheader <- read.table(text = mheader, sep = sep,
                            col.names = c("key", "value"),
                            colClasses = c("character"))

      att[trimws(mheader$key)] <- trimws(mheader$value)
    }

    # we have read one line too much...
    pushBack(oneLine, con)
  }


  x <- read.table(con, col.names = col.names, #colClasses = colClasses,
                  sep = sep, dec = dec, header = header,
                  comment.char = comment.char, ...)
  close(con)


  if(!"discharge" %in% colnames(x))
    stop("There has to be a column named 'discharge'.")


  # read.table() corrects invalid colnames
  x <- unite(x, col = time, make.names(timecols), sep = "-")
  x$time <- as.Date(x$time, format = format)
  x <- validate(x, warn = FALSE, approx.missing = 0)
  attr_smires(x) <- att

  return(x)
}



.read_txt_france <- function(file, parse.header = FALSE,
                             fileEncoding = "ISO8859-14",
                             nlines = -1)
{
  fh <- file(file, open = "rt")#, encoding = fileEncoding)
  header <- readLines(con = fh, n = 3)
  body <- read.csv(file = fh, dec = ".", sep = ";", header = FALSE,
                   as.is = TRUE)
  close(fh)

  body <- body[body[, 1] == "QJO", ]

  x <- data_frame(time = as.Date.character(body[, 3], format = "%Y%m%d"),
                  discharge = as.numeric(body[, 4]))

  meta <- strsplit(header[3], ";")[[1]]
  name <- strsplit(meta[3], split = " \u00e0 | au ")[[1]]

  x <- validate(x, approx.missing = 0, warn = FALSE)
  attr_smires(x) <- list(filename = basename(file),
                         dirname = dirname(file),
                         country = "fr",
                         id = meta[[2]],
                         river = name[1], station = name[2])

  return(x)
}


.read_uk <- function(file, ...)
{
  require(readhyd)
  x <- read.nrfa(file = file)

  y <- as_tibble(x) %>%
    rename(discharge = value) %>%
    validate(approx.missing = 0, warn = FALSE)

  meta <- attr(x, "meta")
  attr_smires(y) <- list(filename = basename(file),
                         dirname = dirname(file),
                         id = meta$eid,
                         river = meta$river, station = meta$station,
                         country = meta$country)

  return(y)
}


.read.es <- function(file, ...)
{
  # monthly data from Francesc

  infile <- read.csv2(file = file)
  year <- infile[, 1]
  month <- as.numeric(substr(colnames(infile)[-1], start = 2L, 10L))

  values <- unname(as.matrix(infile[, -1]))

  months <- matrix(month, nrow = length(year), ncol = 12, byrow = TRUE)
  years <- row(values) + (months < month[1])
  year <- c(year, max(year) + 1)

  data_frame(time = as.Date(paste(year[years], months, 01, sep = "-")),
             discharge = as.vector(values)) %>%
    arrange(time) %>%
    validate(approx.missing = 0, warn = FALSE)
}
