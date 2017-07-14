# todo:
# - assign ids eg: at-01
# - convert coordinates
# - keep track of filenames


library(devtools)
library(smires)
cnames <- c("country", "station", "id", "river", "x", "y", "z", "catchment")
station <- list()
smiresData <- list()


# France -----
# Data from Eric ----
# files <- list.files("ts/fr/SMIRES/France/", full.names = T)
id <- c("H1503910", "H1513210", "H1603010", "H1713010")
files <- paste0("ts/fr/SMIRES/France/", id, "_qj_hydro2.txt")

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

  x <- data.frame(time = as.Date.character(body[, 3], format = "%Y%m%d"),
                  discharge = as.numeric(body[, 4]),
                  stringsAsFactors = FALSE)

  meta <- strsplit(header[3], ";")[[1]]
  name <- strsplit(meta[3], split = " à | au ")[[1]]

  x <- validate(x, approx.missing = 0)
  attr(x, "meta") <- data.frame(filename = basename(file), id = meta[[2]],
                                river = name[1], station = name[2],
                                stringsAsFactors = FALSE)

  return(x)
}


values <- lapply(files, .read_txt_france)
smiresData$fr1 <- values

im <- sapply(values, is.intermittent)
if(any(!im)) stop("Not all intermittent.")

meta <- lapply(values, function(x) attr(x, "meta"))
meta <- do.call(rbind, meta)
meta$country <- "fr"
meta$catchment <- NA
meta$id <- NA
meta$x <- NA
meta$y <- NA
meta$z <- NA


station$fr1 <- as_data_frame(meta) %>%
  select(one_of(cnames)) %>%
  arrange(desc(y))





# Data from Yves ----
# only these stations are intermittent
id <- c(1039, 1046, 1092, 1099, 1101, 1104, 1128, 1140, 1159, 1160, 1162, 1166)
files <- paste0("ts/fr/q_france/EXTRA_QJ_", id, ".txt")

read_france_yves <- function(file, ...)
{
  require(tidyr)
  cnames <- c("year", "month", "day", "discharge", "flag")
  infile <- read.table(file,  col.names = cnames, header = FALSE, skip = 1,
                       colClasses = c(rep("numeric", 4), "NULL"))
  x <- unite(infile, time, year, month, day, sep = "-") %>%
    mutate(time = as.Date(time)) %>%
    validate()

  return(x)
}

values <- lapply(files, read_france_yves)
smiresData$fr2 <- values

im <- sapply(values, is.intermittent)
if(any(!im)) stop("Not all intermittent.")

meta <- read.csv2("ts/fr/q_france/stations.csv", as.is = TRUE)
name <- do.call(rbind, strsplit(meta$name, split = " à | au "))
colnames(name) <- c("river", "station")

meta <- cbind(meta, name)
meta <- meta[match(basename(files), meta$filename), ]
meta$country <- "fr"
meta$id <- NA

station$fr2 <- as_data_frame(meta) %>%
  select(one_of(cnames)) %>%
  arrange(desc(y))



# United Kingdom ----
id <- c(39099, 25022)
files <- paste0("ts/uk/nrfa_public_", id, "_gdf.csv")

read_uk <- function(file, ...)
{
  require(readhyd)
  x <- read.nrfa(file = file)

  y <- as_tibble(x) %>%
    rename(discharge = value) %>%
    validate(approx.missing = 0)

  meta <- attr(x, "meta")
  attr(y, "meta") <- data.frame(filename = basename(file), id = meta$eid,
                                river = meta$river, station = meta$station,
                                country = meta$country,
                                stringsAsFactors = FALSE)

  return(y)
}

values <- lapply(files, read_uk)
smiresData$gb <- values

meta <- lapply(values, function(x) attr(x, "meta"))
meta <- do.call(rbind, meta)
meta$id <- NA
meta$catchment <- NA
meta$x <- NA
meta$y <- NA
meta$z <- NA

station$gb <- as_data_frame(meta) %>%
  select(one_of(cnames)) %>%
  arrange(desc(y))

balder <- values[[which(station$gb$river == "Balder")]]
ampneyBrook <- values[[which(station$gb$river == "Ampney Brook")]]
use_data(balder, ampneyBrook, overwrite = TRUE)


# Cypress ----
infile <- read.csv2("ts/cy/converted.csv",
                    colClasses = c("factor", "Date", "numeric")) %>%
  rename(station = "Location.ID",
         time = "Measurement.Date",
         discharge = "Qmean_m3_s")

levels(infile$station) <- paste0("cy", seq_len(nlevels(infile$station)))

cy1 <- filter(infile, station == "cy1") %>%
  select(time, discharge) %>% validate()

cy2 <- filter(infile, station == "cy2") %>%
  select(time, discharge) %>% validate()

cy3 <- filter(infile, station == "cy3") %>%
  select(time, discharge) %>% validate()

smiresData$cy <- list(cy1, cy2, cy3)

im <- sapply(smiresData$cy, is.intermittent)
if(any(!im)) stop("Not all intermittent.")

# todo: metadata missing

use_data(cy1, cy2, cy3, overwrite = TRUE)

# Spain ----
# Data from Luis ----
id <- c(8060, 9052)
files <- paste0("ts/es/", id, "_Q.txt")

read.es <- function(file, ...)
{
  require(tidyr)
  cnames <- c("station", "day", "month", "year", "discharge")
  infile <- read.table(file,  col.names = cnames, header = FALSE,
                       colClasses = c("NULL", rep("numeric", 4)))
  x <- unite(infile, time, year, month, day, sep = "-") %>%
    mutate(time = as.Date(time)) %>%
    validate()

  return(x)
}


values <- lapply(files, read.es)
smiresData$es1 <- values


meta <- read.csv2("ts/es/metadata.csv")
meta$country <- "es"
meta$station <- NA
meta$river <- NA
meta$id <- NA
meta$z <- NA

station$es1 <- as_data_frame(meta) %>%
  select(one_of(cnames)) %>%
  arrange(desc(y))



# monthly Data from Fracesc ----
# we do not have permission yet, no response
read.es2 <- function(file, ...)
{
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
    validate(approx.missing = 0)
}

values <- read.es2("ts/es/manol.csv")
# smiresData$es2 <- values
#
# station$es2 <- data_frame(country = "es",
#                           station = "Llogaia d'Àlguema",
#                           id = NA,
#                           river = "Riu Manol",
#                           x = NA, y = NA, z = NA,
#                           catchment = NA) %>%
#   select(one_of(cnames)) %>%
#   arrange(desc(y))




# Greece ----
# "ts/gr/all-data.csv" is completely irregular
values <- read.csv2("ts/gr/daily.csv", colClasses = c("Date", "numeric")) %>%
  validate(approx.missing = 0)
smiresData$gr <- values

# GIS system: EGSA (whatever this is...)

station$gr <- data_frame(country = "gr",
                          station = "Vrontamas",
                          id = "GR33",
                          river = "Evrotas",
                          x = 5451347, y = 1636692, z = 140,
                          catchment = 1500) %>%
  select(one_of(cnames)) %>%
  arrange(desc(y))

