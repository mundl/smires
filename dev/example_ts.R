# France -----

files <- list.files("../ts/fr/SMIRES/France/", full.names = T)

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
                      value = body[, 4]))

}

fr <- lapply(files, .read_txt_france)


# United Kingdom ----
require(readhyd)
ampneyBrook <- read.nrfa(file = "../ts/uk//nrfa_public_39099_gdf.csv")
balder <- read.nrfa(file = "../ts/uk//nrfa_public_25022_gdf.csv")
use_data(balder, ampneyBrook)