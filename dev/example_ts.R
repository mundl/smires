# France -----

files <- list.files("ts/fr/SMIRES/France/", full.names = T)

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

  return(data.frame(time = as.Date.character(body[, 3], format = "%Y%m%d"),
                    discharge = body[, 4]))

}

fr <- lapply(files, .read_txt_france)


# United Kingdom ----
require(readhyd)
ampneyBrook <- read.nrfa(file = "ts/uk//nrfa_public_39099_gdf.csv") %>%
  as_tibble() %>% rename(discharge = value) %>% validate()

balder <- read.nrfa(file = "ts/uk//nrfa_public_25022_gdf.csv") %>%
  as_tibble() %>% rename(discharge = value) %>% validate()

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

use_data(cy1, cy2, cy3, overwrite = TRUE)
