basemap <- function(x, extendsEurope = FALSE, minExtend = 1e5, f = 0.2,
                col.land = "white", col.border = "lightgrey",
                col.water = "#C5DAE3")
{


  lonlat <- x[, c("lon", "lat")]
  coord <- transform_crs(x = lonlat$lon, y = lonlat$lat, from = 4326, to = 3034)
  colnames(coord) <- c("x", "y")

  x$x <- coord[, "x"]
  x$y <- coord[, "y"]

  limit <- if(extendsEurope) {
    matrix(c(2923408, 5309418, 1576769, 4855870), nrow = 2,
           dimnames = list(NULL, c("x", "y")))
  } else {
    extend <- max(apply(apply(coord, 2, extendrange, f = f), 2, diff))
    i <- matrix(c(-1, 1, -1, 1), nrow = 2, dimnames = list(NULL, c("x", "y")))

    if(!is.finite(minExtend) | extend > minExtend) {
      minExtend <- extend
    }

    center <- colMeans(apply(coord, 2, range, na.rm = TRUE))
    matrix(center, nrow = 2, ncol = 2, byrow = TRUE) + i * minExtend/2
  }

  ggplot(x, aes(x = x, y = y)) +
    geom_polygon(data = coastlines, aes(x2, y2, group = group),
                 col = col.border, fill = col.land, size = 0.1) +
   # geom_point() +
    coord_fixed(xlim = limit[, "x"], ylim = limit[, "y"]) +
    theme_bw(10) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.background=element_blank(),
          panel.background = element_rect(fill = col.water) ,
          strip.background=element_rect(fill = "white"))
}

