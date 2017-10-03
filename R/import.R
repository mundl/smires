read.metadata <- function(file, as.is = TRUE, header = TRUE, warn = TRUE,
                          encoding = "", ...)
{
  tbl <- read.table(file, as.is = as.is, header = header,
                    fileEncoding = encoding, ...)

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
                        metadata = NA, encoding = "", ...)
{

  if(length(file) > 1) {
    l <- lapply(file, FUN = read.smires, sep = sep, dec = dec,
                col.names = col.names, timecols = timecols,
                format = format,
                comment.char = comment.char, header = header,
                metadata = metadata, encoding = encoding, ...)

    att <- attr_smires(l)
    names(l) <- att$filename

    return(l)
  }

  if(missing(col.names) & !header) {
    col.names <- c("time", "discharge")
    #colClasses <- c(time = "character", discharge = "numeric")
  }

  con <- file(file, encoding = encoding)
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

      # only read first two columns of meta data
      # be relaxed about malformed header
      mheader <- read.table(text = mheader, sep = sep, as.is = TRUE,
                            fill = TRUE)[, 1:2]
      colnames(mheader) <- c("key", "value")

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
  x <- .check_coordinates(x)

  return(x)
}


.check_coordinates <- function(x) {
  m <- attr_smires(x)

  # eliminate empty attributes
  m <- m[lengths(m) > 0]
  m <- m[!(sapply(m, is.na) | sapply(m, is.null))]
  att <- names(m)

  # convert to lon/lat, if not already present
  if(!all(c("lon", "lat") %in% att)) {

    attr_smires(x) <- list(lon = NA, lat = NA)

    # xy coordiantes present and epsg code
    if(all(c("x", "y") %in% att)) {
      if("epsg" %in% att) {
        lonlat <- transform_crs(x = m$x, y = m$y, from = m$epsg, to = 4326)
        attr_smires(x) <- list(lon = lonlat[1, 1], lat = lonlat[1, 2])
      } else {
        warning(shQuote(m$filename), " has xy coordinates but no epsg code.",
                call. = FALSE)
      }
    }
  }


  # check range of lon lat
  m <- attr_smires(x)
  if(is.finite(m$lat) && (m$lat < -90 || m$lat > 90))
    warning(shQuote(m$filename),
            " Latitude must be between -90 and 90. Wrong epsg code?",
            call. = FALSE)

  if(is.finite(m$lon) && (m$lon < -180 || m$lon > 180))
    warning(shQuote(m$filename),
            " Longitude must be between -180 and 180. Wrong epsg code?",
            call. = FALSE)

  return(x)
}


assign_ids <- function(x) {
  if(!is.list(x) || length(x) == 1)
    warning("Assigning unique IDs only makes sense for more than one element")

  att <- attr_smires(x)
  id <- att %>%
    group_by(country) %>%
    mutate(id = sprintf(paste0("%0", nchar(n()), "d"), row_number())) %>%
    unite(col = "id", country, id, sep = "-") %>%
    select(id)

  # store the id in the attribute
  for(i in seq_along(x))
    attr_smires(x[[i]]) <- list(id = id$id[i])

  # name the element of the list with id
  names(x) <- id$id

  return(x)
}




