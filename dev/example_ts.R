# todo:
# - assign ids eg: at-01
# - convert coordinates

library(devtools)
library(smires)
require(tidyr)



# France, Eric -----
# files <- list.files("ts/fr/SMIRES/France/", full.names = T)
id <- c("H1503910", "H1513210", "H1603010", "H1713010")
files <- paste0("ts/fr/SMIRES/France/", id, "_qj_hydro2.txt")


fr1 <- lapply(files, smires:::.read_txt_france)
attr_smires(fr1) <- list(source = "Eric")

# France, Yves -----
id <- c(1039, 1046, 1092, 1099, 1101, 1104, 1128, 1140, 1159, 1160, 1162, 1166)
files <- paste0("ts/fr/q_france/EXTRA_QJ_", id, ".txt")

# station meta data
meta <- read.metadata("ts/fr/q_france/stations.csv", dec = ",", sep = ";")
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
uk <- lapply(files, smires:::.read_uk)



# Cypress ----
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

for(i in names(cy)) attr_smires(cy[[i]]) <- list(id = i)


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


# :-( Spain, Fracesc (monthly data) ----
# we do not have permission yet, no response

es2 <- smires:::.read.es("ts/es/manol.csv")
attr_smires(es2) <- list(country = "es",
                         station = "Llogaia d'Àlguema",
                         river = "Riu Manol")


# Greece ----
# "ts/gr/all-data.csv" is completely irregular

# GIS system: EGSA (whatever this is...)
meta <- list(country = "gr", station = "Vrontamas", id = "GR33",
             river = "Evrotas",
             x = 5451347, y = 1636692, z = 140, catchment = 1500)

gr <- read.smires("ts/gr/daily.csv", sep = ";", dec = ".",
                  metadata = meta)


# Italy, Puglia -----
meta <- read.metadata("ts/it/metadata-it1.csv", sep = ";", dec = ".")
files <- paste0("ts/it/", c("salsola", "celone"), ".csv")

it1  <- read.smires(files, sep = ";", dec = ",", na.strings = "NAV",
                    metadata = meta)


# Italy 2 -----
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

attr_smires(it2) <- list(country = "it", "river" = "Carapelle Torrent")


# Poland ----
pl1 <- read.smires("ts/pl/goryczkowy.csv", sep = ";", dec = ",",
                   metadata = list(country = "pl", source = "1"))


# Poland, Kazimierz ----
infile <- read.table("ts/pl/QPS15.777",
                     col.names = c("hyear", "hmonth", "day", "discharge"))
infile$month <- ((infile$hmonth - 1 + 10) %% 12) + 1
infile$year <- infile$hyear
mask <- infile$hmonth %in% 1:2
infile$year[mask] <- infile$year[mask] - 1

pl2 <- infile%>%
  mutate(time = as.Date(paste(year, month, day, sep = "-"))) %>%
  select(time, discharge) %>%
  validate(approx.missing = 0)

attr_smires(pl2) <- list(country = "pl", source = "Kazimierz")



# Portugal ----
files <- paste0("ts/pt/", c("alentejo", "gamitinha"), ".csv")

pt1 <- read.smires(files, sep = ";", dec = ".",
                   metadata = list(country = "pt", source = "1"))


# Portugal, Teresa ----
meta <- read.metadata(file = "ts/pt/meta.csv", sep = "\t", dec = ".",
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



# Concatenation ----
l <- c(fr1, fr2, uk, cy, es1, list(gr), it1, list(it2),
                list(pl1), list(pl2), pt1, pt2)

noTibble <- which(!sapply(l, tibble::is.tibble))
if(length(noTibble)) stop("Not all elements are already lists.")


smiresData <- l
secret <- c(list(es2))


im <- sapply(smiresData, is.intermittent)
if(any(!im)) stop("Not all intermittent.")

smiresData <- smiresData[im]

station <- attr_smires(smiresData)


# Extract special data sets ----
balder <- smiresData[[which(station$river == "Balder")]]
ampneyBrook <- smiresData[[which(station$river == "Ampney Brook")]]
use_data(balder, ampneyBrook, overwrite = TRUE)


cy1 <- cy[["r3-7-1-50"]]
cy2 <- cy[["r7-2-3-50"]]
cy3 <- cy[["r8-5-1-60"]]


use_data(cy1, cy2, cy3, overwrite = TRUE)











