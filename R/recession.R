# example from Prof. Ripley, mod. M Maechler
.peaks <- function (series, span = 3) {
  if ((span = as.integer(span))%%2 != 1) stop("'span' must be odd")

  s1 = 1:1 + (s = span%/%2)
  z = embed(series, span)
  v = apply(z[, s1] > z[, -s1, drop = FALSE], 1, all)

  pad = rep.int(FALSE, s)
  return(c(pad, v, pad))
}

.splitAt <- function(x, pos) {
  unname(split(x, findInterval(seq_along(x), pos)))
}

.rsegment <- function(x, span = 5, smooth = 0) {
  # cut the series at the flood peaks
  y <- as.vector(x)
  idxPeak <- which(.peaks(y, span = span))
  seg <- .splitAt(y, idxPeak)

  # only keep limb if flow is decreasing
  lapply(seg, .monotonic, smooth = smooth, strict = TRUE)
}


# returns the first elements which meet the required monotonicity
.monotonic <- function(x, falling = TRUE, strict = TRUE, smooth = 0)
{
  if(smooth != 0) warning("Argument 'smooth' is currently ignored.")

  # NAs are assumed to not change the monotonicity, approximate them
  y <- approx(x = seq_along(x), y = x, xout = seq_along(x))$y

  # always keep first element
  delta <- sign(diff(y))
  delta <- c(delta[1], delta)

  candidates <- if(falling) -1 else 1
  if(!strict) candidates <- c(candidates, 0)

  until <- which(!delta %in% candidates)[1] - 1

  if(is.na(until)) {
    # the whole vector x is of the required monotocity
    x
  }  else {
    head(x, until)
  }
}


.sanititze_segment <- function(x, length = c(4, 7), drop.first = 2,
                              cut.at.NA = FALSE, constant = TRUE,
                              threshold = NULL) {

  y <- tail(x, -drop.first)
  if(!is.null(threshold))   y <- y[y < threshold]

  # eliminate segments which are constant
  if(!constant & length(table(y)) == 1) return(NULL)

  if(cut.at.NA) y <- head(y, min(c(which(is.na(y)), length(y))))

  if(length(y) >= length[1]) return(head(y, length[2]))
}

# equivalend to embed(, dim = 2), but with colnames
.rpair <- function(x) cbind("t0" = head(x, -1), "t1" = tail(x, -1))


recession <- function(x, span = 5, smooth = 0, length = c(4, 7), drop.first = 2,
                cut.at.NA = TRUE, threshold = median(x, na.rm = TRUE)) {

  seg <- .rsegment(x = x, span = span, smooth = smooth)

  seg <- lapply(seg, .sanititze_segment, length = length, drop.first = drop.first,
                cut.at.NA = cut.at.NA, threshold = threshold, constant = FALSE)

  seg <- seg[!sapply(seg, is.null)]

  pair <- lapply(seg, .rpair)

  # fit a model for each recession curve
  model <- lapply(pair, function(x) lm(t1 ~ t0 + 0, as.data.frame(x)))

  # extract just the slope
  k <- sapply(model, function(x) unname(coef(x)["t0"]))

  return(k)
}
