#' Extract file extension from filename
#'
#' @param path A string of path to a file.
#'
#' @returns File extension extracted from the input string.
#' @export
#'
#' @examples
#' extract_extension("my_file.ext")
extract_extension <- function(path) {
  sep <- strsplit(basename(path), split = "\\.")[[1]]
  sep[length(sep)]
}

#' Check and annotate the ordering of the input
#'
#' @description
#' The function can be used to check the sweeping mode with V_gate, which is
#' supposed to be in ascending order (forward sweeping) and then descending
#' order (backward sweep). It returns a list of two indexing vectors named
#' `fwd` and `bwd`.
#'
#' @param x A numeric vector.
#'
#' @returns A list of two indexing vectors named `fwd` and `bwd`.
#' @export
#'
#' @examples
#' index_sweep(c(seq(0, 0.5, by = 0.05), seq(0.45, 0, by = -0.05)))
index_sweep <- function(x) {
  n <- length(x)
  d <- diff(x)

  # Single sweep, then two sweeps
  if (all(d < 0)) {
    # Backward sweep only
    return(list(bwd = rev(1:n)))
  } else if (all(d > 0)) {
    # Forward sweep only
    return(list(fwd = 1:n))
  } else {
    idx_max <- which.max(x)
    # Check for expected pattern: min -> max -> min
    if (all(d[1:(idx_max - 1)] > 0) && all(d[idx_max:(n - 1)] < 0)) {
      return(list(fwd = 1:idx_max, bwd = rev(idx_max:n)))
    } else if (all(d[1:(idx_max - 1)] > 0) && all(d[(idx_max + 1):(n - 1)] < 0)) {
      return(list(fwd = 1:idx_max, bwd = rev((idx_max + 1):n)))
    }

    # Check for less common pattern: max -> min -> max
    if (idx_max %in% c(1, n)) {
      idx_min <- which.min(x)
      if (all(d[1:(idx_min - 1)] < 0) && all(d[idx_min:(n - 1)] > 0)) {
        return(list(bwd = rev(1:idx_min), fwd = idx_min:n))
      } else if (all(d[1:(idx_min - 1)] < 0) && all(d[(idx_min + 1):(n - 1)] > 0)) {
        return(list(bwd = rev(1:idx_min), fwd = (idx_min + 1):n))
      }
    }
  }

  # Return NULL for unexpected pattern
  NULL
}

#' Numerical differentiation
#'
#' @description
#' Numerical differentiation to approximate the derivative \eqn{dy/dx}.
#'
#' @param x A numeric vector \eqn{x}.
#' @param y A numeric vector \eqn{y=f(x)}.
#'
#' @returns A numeric vector for the approximate derivative \eqn{dy/dx}.
#' @export
#'
#' @examples
#' set.seed(1)
#' num_diff(1:10, 3 * (1:10) + rnorm(10))
num_diff <- function(x, y) {
  if (length(x) != length(y))
    stop("x and y must have equal length")

  if (any(duplicated(x)))
    stop("x cannot have duplicated values")

  # x should be in ascending order
  n <- length(x)
  dydx <- vector("double", n)
  for (i in 2:(n - 1)) {
    # Centered difference for the interior points
    dydx[i] <- (y[i + 1] - y[i - 1]) / (x[i + 1] - x[i - 1])
  }
  dydx[1] <- (y[2] - y[1]) / (x[2] - x[1]) # Forward at left
  dydx[n] <- (y[n] - y[n - 1]) / (x[n] - x[n - 1])  # Backward at right

  dydx
}

#' Moving average
#'
#' @param x A numeric vector.
#' @param n An integer to indicate the number of points to be averaged.
#'
#' @returns A numeric vector of the averaged result.
#' @export
#'
#' @examples
#' moving_ave(1:20, n = 3)
moving_ave <- function(x, n = 5) {
  as.double(stats::filter(x, rep(1 / n, n), sides = 2))
}

#' Average of numerical difference
#'
#' @description
#' A wrapper of `num_diff()` and `moving_ave` to find smoothed derivative
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param n An integer to indicate the number of points to be averaged.
#'
#' @returns A numeric vector of the averaged derivative.
#' @export
averaged_diff <- function(x, y, n = 5) {
  dydx <- num_diff(x, y)
  moving_ave(dydx, n)
}
