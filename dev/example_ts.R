library(devtools)
library(smires)
require(tidyr)

.remove_na_dups <- function(x)
{
  dups <- duplicated(x$time) | duplicated(x$time, fromLast=TRUE)
  x[!(dups & is.na(x$discharge)), ]
}

.remove_dup_index <- function(x)
{
  x[!duplicated(x$time, fromLast = TRUE), ]
}

.trim_eclosing_na <- function(x)
{
  na <- !is.na(x$discharge)
  mask <- cumsum(na) > 0 & rev(cumsum(rev(na)) > 0)
  x[mask, ]
}

.read_at <- function(file, ...)
{
  require(readhyd)
  require(lubridate)
  x <- read.hzb(file = file)

  y <- as_tibble(x) %>%
    rename(discharge = "value") %>%
    mutate(time = as.Date(floor_date(time, unit = "day"), tz = "GMT")) %>%
    distinct() %>%
    .remove_na_dups() %>%
    .remove_dup_index() %>%
    .trim_eclosing_na() %>%
    validate(approx.missing = 0, warn = FALSE)

  metaL <- attr(x, "list")
  meta <- attr(x, "keyval")
  id <- meta[meta[, 1] == "HZB-Nummer", 2]

  attr_smires(y) <- list(filename = basename(file),
                         dirname = dirname(file),
                         id = id,
                         river = meta[meta[, 1] == "Gewässer", 2],
                         station = meta[meta[, 1] == "Messstelle", 2],
                         country = "at",
                         catchment = as.numeric(meta[meta[, 1] == "orogr.Einzugsgebiet", 2]),
                         altitude = as.numeric(tail(metaL$`Pegelnullpunkt`[, "Höhe"], 1)),
                         lon = dms2dec(tail(metaL$`Geographische Koordinaten`[, "Länge"], 1)),
                         lat = dms2dec(tail(metaL$`Geographische Koordinaten`[, "Breite"], 1)),
                         epsg = 4326,
                         provider = "http://ehyd.gv.at/",
                         link = paste0("http://ehyd.gv.at/eHYD/MessstellenExtraData/owf?id=", id, "&file=4"))

  return(y)
}


.read_txt_france <- function(file, parse.header = FALSE,
                             fileEncoding = "ISO8859-14",
                             nlines = -1)
{
  fh <- file(file, open = "rt", encoding = fileEncoding)
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



# France, Eric -----
# files <- list.files("ts/fr/SMIRES/France/", full.names = T)
id <- c("H1503910", "H1513210", "H1603010", "H1713010")
files <- paste0("ts/fr/SMIRES/France/", id, "_qj_hydro2.txt")

fr1 <- lapply(files, .read_txt_france, fileEncoding = "")
attr_smires(fr1) <- list(source = "Eric")

# France, Yves -----
id <- c(1039, 1046, 1092, 1099, 1101, 1104, 1128, 1140, 1159, 1160, 1162, 1166)
files <- paste0("ts/fr/q_france/EXTRA_QJ_", id, ".txt")

# station meta data
meta <- read.metadata("ts/fr/q_france/stations.csv", dec = ",", sep = ";")#,
#encoding = "ISO8859-14")
meta$epsg <- 27572
meta <- separate(meta, name, into = c("river", "station"),
                 sep = " \u00e0 | au ") %>%
  mutate(country = "fr", source = "Yves")

fr2 <- read.smires(files,
                   col.names = c("year", "month", "day", "discharge", "flag"),
                   header = FALSE, skip = 1,
                   sep = "",
                   colClasses = c(rep("numeric", 4), "NULL"),
                   timecols = c("year", "month", "day"),
                   metadata = meta)


# United Kingdom ----
id <- c(39099, 25022)
files <- paste0("ts/uk/nrfa_public_", id, "_gdf.csv")
uk <- lapply(files, read_uk)
attr_smires(uk) <- list(source = "Cath")


# Cypress ----
meta <- read.csv2("ts/cy/meta.csv", as.is = TRUE) %>%
  mutate(country = "cy", source = "Gerald",
         provider = "Cyprus Water Development Department")

file <- "ts/cy/converted.csv"
infile <- read.csv2(file =  file,
                    colClasses = c("factor", "Date", "numeric")) %>%
  rename(station = "Location.ID",
         time = "Measurement.Date",
         discharge = "Qmean_m3_s")

infile <- split(infile[, c("time", "discharge")], f = infile$station)
station <- names(infile)

cy <- lapply(infile, validate, approx.missing = 0, warn = FALSE)
attr_smires(cy) <- list(filename = basename(file),
                        dirname = dirname(file),
                        country = "cy")

for(i in names(cy)) attr_smires(cy[[i]]) <- as.list(meta[meta$id == i, ])


# Spain, Luis ----
id <- c(8060, 9052)
files <- paste0("ts/es/", id, "_Q.txt")

meta <- read.metadata("ts/es/metadata.csv", sep = ";", dec = ".")
meta <- mutate(meta, country = "es", source = "Luis")

es1 <- read.smires(files, header = FALSE, sep = "\t",
                   col.names = c("station", "day", "month", "year", "discharge"),
                   colClasses = c("NULL", rep("numeric", 4)),
                   timecols = c("year", "month", "day"),
                   metadata = meta)
attr_smires(es1) <- list(lon = NA, lat = NA)


# :-( Spain, Fracesc (monthly data) ----
# we do not have permission yet, no response

es2 <- .read.es("ts/es/manol.csv")
attr_smires(es2) <- list(country = "es",
                         station = "Llogaia d'Àlguema",
                         river = "Riu Manol")


# Greece ----
# "ts/gr/all-data.csv" is completely irregular

# GIS system: EGSA (whatever this is...)
meta <- list(country = "gr", station = "Vrontamas", id = "GR33",
             river = "Evrotas",
             x = 5451347, y = 1636692, z = 140, catchment = 1500,
             source = "Rania")

gr <- read.smires("ts/gr/daily.csv", sep = ";", dec = ".",
                  metadata = meta)
gr$discharge <- gr$discharge / 1000


# Italy, Guiseppe Puglia -----
meta <- read.metadata("ts/it/metadati.csv", sep = ";", dec = ",") %>%
  mutate(source = "Guiseppe")
# meta <- read.metadata("ts/it/metadata-it1.csv", sep = ";", dec = ".") %>%
#   mutate(country = "it", source = "Guiseppe")
files <- paste0("ts/it/", c("salsola", "celone"), ".csv")

it1  <- read.smires(files, sep = ";", dec = ",", na.strings = "NAV",
                    metadata = meta)


# Italy, Annamaria  -----
files <- "ts/it/carapelle-torrent.csv"

# this file is in wide format
infile <- read.csv2(files, as.is = TRUE, check.names = FALSE)
infile$month <- match(infile$month, month.name)

it2 <- gather(infile, key = year, value = discharge, -month, -day) %>%
  mutate(time = as.Date(paste(year, month, day, sep = "-"))) %>%
  select(time, discharge) %>%
  # every year has a cell for Feb 29th
  filter(!is.na(time)) %>%
  validate(approx.missing = 0, warn = FALSE)

meta2 <- filter(meta, filename == "carapelle-torrent.csv")
attr_smires(it2) <- c(as.list(meta2),
                      list(dirname = dirname(files),
                           source = "Annamaria"))

it2 <- smires:::.check_coordinates(it2)


# Poland, Agnieszka ----
pl1 <- read.smires("ts/pl/goryczkowy.csv", sep = ";", dec = ",",
                   metadata = list(country = "pl", source = "Agnieszka"))


# Poland, Kazimierz ----
files <- "ts/pl/QPS15.777"
infile <- read.table(files,
                     col.names = c("hyear", "hmonth", "day", "discharge"))
infile$month <- ((infile$hmonth - 1 + 10) %% 12) + 1
infile$year <- infile$hyear
mask <- infile$hmonth %in% 1:2
infile$year[mask] <- infile$year[mask] - 1

pl2 <- infile%>%
  mutate(time = as.Date(paste(year, month, day, sep = "-"))) %>%
  select(time, discharge) %>%
  validate(approx.missing = 0)

attr_smires(pl2) <- list(country = "pl", source = "Kazimierz",
                         hydrological.year = "November",
                         filename = basename(files),
                         dirname = dirname(files))



# Portugal, Helena ----
meta <- read.metadata(file = "ts/pt/meta-helena.csv", sep = ";", dec = ".",
                      header = TRUE, warn = FALSE) %>%
  mutate(country = "pt", source = "Helena")

files <- paste0("ts/pt/", c("alentejo", "gamitinha"), ".csv")

pt1 <- read.smires(files, sep = ";", dec = ".", metadata = meta)
for (i in names(pt1)) pt1[[i]]$discharge <- pt1[[i]]$discharge / 1000


# Portugal, Teresa ----
meta <- read.metadata(file = "ts/pt/runoff/meta-teresa.csv", sep = "\t", dec = ".",
                      warn = FALSE) %>%
  mutate(country = "pt", source = "Teresa")

files <- file.path("ts/pt/runoff/", meta$filename)

pt2 <- read.smires(
  file = files,
  skip = 4, sep = "\t",
  colClasses = c("character", rep("NULL", 2), "numeric", rep("NULL", 4)),
  col.names = c("time", "yearly", "flag.y", "discharge", "flag.d",
                "monthly", "flag.m", "empty"),
  format = "%d/%m/%Y",
  metadata = meta)

# Swiss, Ilja ----
ch <- read.smires("ts/ch/04-Altlandenberg_mod.txt", header = TRUE, sep = "\t",
                  col.names = c("time", "discharge", "perc",
                                "MaxWert", "MaxZeit",	"MinWert", "MinZeit"),
                  colClasses = c("character", "numeric", rep("NULL", 5)),
                  format = "%d.%m.%Y")


# Slovenia, Simon ----
meta <- read.metadata(file = "ts/si/meta.csv", sep = ";", dec = ",",
                      encoding = "UTF-8", warn = FALSE) %>%
  mutate(country = "si", source = "Simon")

files <- file.path("ts/si/runoff", c("1100.csv", "1300.csv", "1310.csv",
                                     "3400.csv", "8680.csv", "9280.csv",
                                     "9300.csv"))

si <- read.smires(file = files, sep = ";", dec = ",", metadata = meta)

# :-( Slovakia, Silvia ----
files <- list.files(path = "ts/sk/", pattern = "\\.csv", full.names = TRUE)
sk <- read.smires(files, sep = ";", dec = ",", format = "%d.%m.%Y",
                  encoding = "UTF-8")

attr_smires(sk) <- list(country = "sk", source = "Silvia")


# Austria ----
files <- list.files(path = "ts/at/", pattern = "\\.csv", full.names = TRUE)
at <- lapply(files, .read_at)
attr_smires(at) <- list(country = "at", source = "Tobias")


# Concatenation ----
l <- c("Eric" = fr1,
       "Yves" = fr2,
       "CEH" = uk,
       "Gerald" = cy,
       "Luis" = es1,
       "Rania" = list(gr),
       "Guiseppe" = it1,
       "Annamaria" = list(it2),
       "Agnieszka" = list(pl1),
       "Kasimierez" = list(pl2),
       "Helena" = pt1,
       "Teresa" = pt2,
       "Simon" = si,
       "Tobias" = at)

noTibble <- which(!sapply(l, tibble::is.tibble))
if(length(noTibble)) stop("Not all elements are already lists.")


smires <- enframe_smires(l)
secret <- c(list(es2), sk)


im <- sapply(smires$data, is.intermittent)
if(any(!im)) warning("Found ", sum(!im), " non-intermittent rivers: ",
                     paste(smires$sid[!im], collapse = ", "))

smires$intermittent <- im

mask <- is.na(smires$z)
smires$z[mask] <- smires$elevation[mask]

mask <- is.na(smires$altitude)
smires$altitude[mask] <- smires$z[mask]

cols <- c("sid", "data", "country", "id", "river", "station", "catchment",
          "lon", "lat", "altitude", "intermittent", "x", "y","epsg", "provider", "link")

smires <- smires[, cols]
use_data(smires, overwrite = TRUE)

# Extract special data sets ----
balder <- smires$data[[which(smires$river == "Balder")]]
ampneyBrook <- smires$data[[which(smires$river == "Ampney Brook")]]
use_data(balder, ampneyBrook, overwrite = TRUE)
