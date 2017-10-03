transform_crs <- function(x, y, from, to)
{
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

has_coordinates <- function(x) {
  # simply checks if lat/lon attribute is present
  complete.cases(attr_smires(x)[, c("lat", "lon")])
}
