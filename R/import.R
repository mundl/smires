read.metadata <- function(file, as.is = TRUE, header = TRUE, ...)
{
  tbl <- read.table(file, as.is = as.is, header = header, ...)

  if(!"filename" %in% colnames(tbl)) {
    warning("There is no column called 'filename' in '", basename(file), "'. ")
    tbl <- NA
  } else {
    invalidFilename <- is.na(tbl$filename) | tbl$filename == ""
    if(any(invalidFilename)) {
      tbl <- tbl[!invalidFilename, ]
      warning("Dropping ", sum(invalidFilename),
              " rows with invalid file names from '", basename(file), "'.",
              call. = FALSE)
    }

  }

  # todo: probably check for valid colnames
  # cnames <- c("country", "station", "id", "river", "x", "y", "z", "catchment")

  return(tbl)
}

read.smires <- function(file,
                        sep = ",", dec = ".",
                        col.names = c("time", "discharge"),
                        colClasses = c(time = "character", discharge = "numeric"),
                        format = "%Y-%m-%d",
                        comment.char = "#", header = TRUE,
                        metadata = NA, ...)
{

  if(length(file) > 1) {
    l <- lapply(file, FUN = read.smires, sep = sep, dec = dec, col.names = col.names,
                colClasses = colClasses, format = format,
                comment.char = comment.char, header = header,
                metadata = metadata, ...)

    att <- attr_smires(l)
    names(l) <- att$filename

    return(l)
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
              "' in the table of meta data '", basename(metadata), "' found.",
              "\nTrying to fetch meta data information from header of '",
              basename(file), "'")
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


  x <- read.table(con, col.names = col.names, colClasses = colClasses,
                  sep = sep, dec = dec, header = header,
                  comment.char = comment.char, ...)
  close(con)

  x$time <- as.Date(x$time, format = format)

  x <- validate(x, warn = FALSE, approx.missing = 0)
  attr_smires(x) <- att

  return(x)
}
