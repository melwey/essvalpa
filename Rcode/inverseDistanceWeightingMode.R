inverseDistanceWeighting <- function(nrow = 17, ncol = nrow, distm = 250){
  m <- matrix(nrow = nrow, ncol = ncol)
  centre_row <- ceiling(nrow/2)
  centre_col <- ceiling(ncol/2)
  for (i in 1 : ncol){
    for (j in 1 : nrow){
      d_x <- abs(centre_col - i) * distm
      d_y <- abs(centre_row - j) * distm
      m[ j, i ] <- 1 / ( d_x^2 + d_y^2)
    }
  }
  m[centre_row, centre_col] <- NA
  return(round(m / min(m, na.rm = TRUE)))
}

inverseDistanceWeightingMode <- function(x, na.rm = TRUE){
  # rep each element of x by the element of inverseDistanceWeighting with same coordinates
  m <- inverseDistanceWeighting()
  y <- NA
  for (i in 1 : length(x)){
    if (!is.na(m[i])){
      y <- c(y, rep(x[i], m[i]))
    }
  }
  return(modal(y, na.rm = na.rm))
}
