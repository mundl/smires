transform_crs <- function(x, y, from, to)
{
  len <- lengths(list(from = from, to = to))

  if(any(len > 1)) {
    if(!all(len %in% c(1, length(x)))) {
      stop("Argument 'from' and 'to' must be of length 1 or ", length(x), ".")
    }
    data <- data.frame(x = x, y = y, from = from, to = to, row.names = seq_along(x))

    g <- data[, c("from", "to")]

    z <- by(data, g, function(x) data.frame(transform_crs(x$x, x$y,
                                                          from = unique(x$from),
                                                          to = unique(x$to)),
                                            id = as.numeric(rownames(x))))
    z <- do.call(rbind, z)
    z <- as.matrix(z[order(z$id), 1:2])
    return(unname(z))
  } else {
    require(sf)
    xy <- data.frame(x, y)

    # we cannot transform NA values
    transformed <- matrix(NA, nrow = nrow(xy), ncol = 2)
    mask <- complete.cases(xy)
    xy <- xy[mask, ]

    if(nrow(xy) > 0) {
      p1 <- st_as_sf(xy, coords = c("x", "y"), crs = from)
      p2 <- st_transform(p1, crs = to)
      transformed[mask, ] <- do.call(rbind, st_geometry(p2))
    }

    return(transformed)
  }
}

has_coordinates <- function(x) {
  # simply checks if lat/lon attribute is present
  complete.cases(attr_smires(x)[, c("lat", "lon")])
}
