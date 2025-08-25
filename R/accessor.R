#' @method print OTDataSet
#' @export
print.OTDataSet <- function(x, n = 6L, ...) {
  iv <- x[["IVData"]]
  m <- x[["metadata"]]

  cat("OTDataSet: A container (S3 class) for Organic Transistor Data Set with the following information: \n")
  cat("  IVData:   n=", length(iv), ", each has variables ", paste0(names(iv[[1]]), collapse = ", "), "\n", sep = "")
  cat("  metadata: ", paste0(names(m), collapse = ", "), "\n", sep = "")
  cat("  units:    ", attr(x, "units"), "\n", sep = "")
  cat("\n")
  cat("Number of transistors for each level of each of the following metadata variables: \n")
  cat("  - substrate: ", paste0(names(table(m$substrate)), " (", table(m$substrate), ")", collapse = "; "), "\n", sep = "")
  cat("  - length:    ", paste0(names(table(m$length)), " (", table(m$length), ")", collapse = "; "), "\n", sep = "")
  cat("  - width:     ", paste0(names(table(m$width)), " (", table(m$width), ")", collapse = "; "), "\n", sep = "")
  cat("  - thickness: ", paste0(names(table(m$thickness)), " (", table(m$thickness), ")", collapse = "; "), "\n", sep = "")
  cat("  - type:      ", paste0(names(table(m$type)), " (", table(m$type), ")", collapse = "; "), "\n", sep = "")
  cat("\n")
  cat(paste0("Annotations (metadata) for the first ", n, " transistors: \n", sep = ""))
  print(head(m), n, ...)
}

#' @method print OTAnalysis
#' @export
print.OTAnalysis <- function(x, n = 6L, ...) {
  iv <- x[["IVData"]]
  m <- x[["metadata"]]
  sres <- x[["sweepResult"]]
  ores <- x[["OTResult"]]

  cat("OTAnalysis: An extended container (S3 class) for OTDataSet, with sweep- and transistor-level analysis results \n")
  cat("  IVData: n=", length(iv), ", each has variables ", paste0(names(iv[[1]]), collapse = ", "), "\n", sep = "")
  cat("  metadata: ", paste0(names(m), collapse = ", "), "\n", sep = "")
  cat("  sweepResult: Sweep-level results for each of the n=", length(sres$sweepData), " transistors \n", sep = "")
  cat("    -- sweepData: ", paste0(names(sres$sweepData[[1]]), collapse = ", "), "\n", sep = "")
  cat("    -- QC: measures and flags for ",
      paste0(unique(sres$QC[[1]]$term), collapse = ", "), "\n", sep = "")
  cat("    -- estimate: ", paste0(names(sres$estimate[[1]]), collapse = ", "), " \n", sep = "")
  cat("  OTResult: Transistor-level results in terms of ", paste0(names(ores[[1]]), collapse = ", "), " \n", sep = "")
  cat("  units: ", attr(x, "units"), "\n", sep = "")
  cat("\n")
  cat("Number of transistors for each level of each of the following metadata variables: \n")
  cat("  - substrate: ", paste0(names(table(m$substrate)), " (", table(m$substrate), ")", collapse = "; "), "\n", sep = "")
  cat("  - length:    ", paste0(names(table(m$length)), " (", table(m$length), ")", collapse = "; "), "\n", sep = "")
  cat("  - width:     ", paste0(names(table(m$width)), " (", table(m$width), ")", collapse = "; "), "\n", sep = "")
  cat("  - thickness: ", paste0(names(table(m$thickness)), " (", table(m$thickness), ")", collapse = "; "), "\n", sep = "")
  cat("  - type:      ", paste0(names(table(m$type)), " (", table(m$type), ")", collapse = "; "), "\n", sep = "")
  cat("\n")
  cat(paste0("Annotations (metadata) for the first ", n, " transistors: \n", sep = ""))
  print(head(m))
}

#' @method [ OTDataSet
#' @export
`[.OTDataSet` <- function(x, i, ...) {
  iv <- x[["IVData"]]
  meta <- x[["metadata"]]
  u <- attr(x, "units")

  if (!all(i %in% seq_along(iv))) {
    stop("Index out of bounds")
  }
  OTDataSet(iv[i], meta[i, ], u)
}

#' @method [ OTAnalysis
#' @export
`[.OTAnalysis` <- function(x, i, ...) {
  iv <- x[["IVData"]]
  m <- x[["metadata"]]
  sres <- x[["sweepResult"]]
  ores <- x[["OTResult"]]
  u <- attr(x, "units")

  if (!all(i %in% seq_along(iv))) {
    stop("Index out of bounds")
  }
  sres <- list(
    sweepData = sres$sweepData[i],
    QC = sres$QC[i],
    estimate = sres$estimate[i]
  )
  validate_OTAnalysis(new_OTAnalysis(iv[i], meta[i, ], sres, ores[i], u))
}

#' @method head OTDataSet
#' @export
head.OTDataSet <- function(x, n = 6L, ...) {
  utils::head(x[["metadata"]], n, ...)
}

#' @method tail OTDataSet
#' @export
tail.OTDataSet <- function(x, n = 6L, ...) {
  utils::tail(x[["metadata"]], n, ...)
}

#' @method units OTDataSet
#' @export
units.OTDataSet <- function(x) {
  attr(x, "units")
}
