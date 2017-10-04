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

read.nrfa <- function(file, nlines = -1, ...) {
  txt <-  readLines(file)

  lines.header <- grep("data,last,", head(txt, 50), fixed = T)
  header <- head(txt, lines.header)

  infile <- read.csv(text = txt, header = F, skip = lines.header,
                     col.names = c("time", "value", "flag"),
                     colClasses = c("Date", "numeric", "NULL"),
                     strip.white = TRUE, as.is = TRUE, nrow = nlines)


  y <- infile[, c("time", "value")]


  meta <- .parse_header_nrfa(header)
  coord <- grid2wgs(meta$station$gridReference)

  attr(y, "meta") <- list(
    eid = meta$station$id,
    river = sub(" at .+$", "", meta$station$name),
    station = sub("^.+ at ", "", meta$station$name),
    unit = sub("3", "^3", meta$dataType$units, fixed = TRUE),
    institution = meta$database$name,
    country = "gb",
    lon = coord[1, "lon"],
    lat = coord[1, "lat"],
    epsg = 4326,
    time = as.Date(unlist(meta$data[c("first", "last")], use.names = F)))

  return(y)
}

.parse_header_nrfa <- function(x, ...) {

  y <- read.csv(text = x, header = F, strip.white = TRUE,
                col.names = c("key", "component", "value"),
                colClasses = c("character"), ...)

  meta <- list()
  for(i in seq_len(nrow(y))) {
    meta[[y$key[i]]][[y$component[i]]] <- y$value[i]
  }

  return(lapply(meta, as.list))

}


read_uk <- function(file, ...)
{
  x <- read.nrfa(file = file)

  y <- as_tibble(x) %>%
    rename(discharge = "value") %>%
    validate(approx.missing = 0, warn = FALSE)

  meta <- attr(x, "meta")
  attr_smires(y) <- list(filename = basename(file),
                         dirname = dirname(file),
                         id = meta$eid,
                         river = meta$river, station = meta$station,
                         country = meta$country,
                         lon = meta$lon,
                         lat = meta$lat,
                         epsg = meta$epsg)

  return(y)
}




grid2wgs <- function (grid)
{
  letter <- strsplit(substr(grid, 1L, 2L), split = "", fixed = TRUE)
  letter <- do.call(rbind, letter)

  # Ireland has different CRS
  epsg <- ifelse(letter[, 1] == "I", 29902, 27700)

  offset1 <- list("S" = c(x = 0, y = 0), "T" = c(5, 0),
                  "N" = c(0, 5), "H" = c(0, 10),
                  "O" = c(5, 5), "I" = c(0, 0))
  offset1 <- do.call(rbind, offset1)

  offset2 <- list(
    "A" = c(y = 4, x = 0), "B" = c(4, 1), "C" = c(4, 2), "D" = c(4, 3), "E" = c(4, 4),
    "F" = c(3, 0), "G" = c(3, 1), "H" = c(3, 2), "J" = c(3, 3), "K" = c(3, 4),
    "L" = c(2, 0), "M" = c(2, 1), "N" = c(2, 2), "O" = c(2, 3),"P" = c(2, 4),
    "Q" = c(1, 0), "R" = c(1, 1), "S" = c(1, 2), "T" = c(1, 3), "U" = c(1, 4),
    "V" = c(0, 0), "W" = c(0, 1), "X" = c(0, 2), "Y" = c(0, 3), "Z" = c(0, 4))
  offset2 <- do.call(rbind, offset2)[, c("x", "y")]

  offset <- offset1[letter[, 1], , drop = FALSE] +
    offset2[letter[, 2], , drop = FALSE]

  padz <- function(x, n=max(nchar(x))) gsub(" ", "0", formatC(x, width=-n))

  # extract x and y parts, pad with trailing zeros if precision is low
  n <- nchar(grid) - 2
  x <- paste0(offset[, "x"], padz(substr(grid, 3, (n/2) + 2), n = 5))
  y <- paste0(offset[, "y"], padz(substr(grid, (n/2) + 3, n + 2), n = 5))

  # cat("x: ", x, fill = TRUE)
  # cat("y: ", y, fill = TRUE)

  xy <- transform_crs(x = as.numeric(x), y = as.numeric(y),
                      from = epsg, to = 4326)
  colnames(xy) <- c("lon", "lat")

  return(xy)
}
