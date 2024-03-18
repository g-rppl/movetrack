# Convert degrees to radians
.toRad <- function(deg) {
  return(deg * pi / 180)
}

# Convert radians to degrees
.toDeg <- function(rad) {
  return(rad * 180 / pi)
}

# Circular difference
.circDiff <- function(x, y) {
  180 - abs(abs(x - y) - 180)
}

# Distance between geographic points
.distGeo <- function(lon1, lat1, lon2, lat2, r = 6378.137) {
  dLon <- .toRad(lon2 - lon1)
  dLat <- .toRad(lat2 - lat1)

  lat1 <- .toRad(lat1)
  lat2 <- .toRad(lat2)

  a <- sin(dLat / 2) * sin(dLat / 2) +
    sin(dLon / 2) * sin(dLon / 2) * cos(lat1) * cos(lat2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  return(r * c)
}

# Destination given bearing (direction) and distance
.destPoint <- function(lon1, lat1, b, d, r = 6378.137) {
  lon1 <- .toRad(lon1)
  lat1 <- .toRad(lat1)
  a <- .toRad(b)

  lat2 <- asin(sin(lat1) * cos(d / r) + cos(lat1) * sin(d / r) * cos(a))
  lon2 <- lon1 + atan2(
    sin(a) * sin(d / r) * cos(lat1),
    cos(d / r) - sin(lat1) * sin(lat2)
  )
  return(cbind(
    lon = .toDeg(lon2),
    lat = .toDeg(lat2)
  ))
}

# Calculate lagged distances in metres from posterior draws
.distance <- function(lon, lat, index) {
  dist <- array(NA, dim(lon), dimnames(lon))
  for (i in 2:dim(lon)[3]) {
    dist[, , i] <- .distGeo(
      lon[, , i], lat[, , i],
      lon[, , i - 1], lat[, , i - 1]
    )
  }
  dist[, , index[-c(length(index))] + 1] <- NA
  dimnames(dist)$variable <- paste0("distance[", seq_len(dim(lon)[3]), "]")
  class(dist) <- c("draws_array", "draws", "array")
  return(dist * 1e3)
}

# Calculate lagged speeds in metres per second from posterior draws
.speed <- function(distance, time) {
  speed <- sweep(distance, 3, (c(NA, diff(time)) * 60), "/")
  dimnames(speed)$variable <- paste0("speed[", seq_len(dim(distance)[3]), "]")
  return(speed)
}
