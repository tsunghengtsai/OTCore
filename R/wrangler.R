# OTDataSet ---------------------------------------------------------------

#' @method nest OTDataSet
#' @export
nest.OTDataSet <- function(.data, ...) {
  m <- .data[["metadata"]]
  iv <- .data[["IVData"]]
  tibble(m, data = iv)
}

#' @method as.data.frame OTDataSet
#' @export
as.data.frame.OTDataSet <- function(x, ...) {
  y <- nest.OTDataSet(x)
  unnest(y, .data$data)
}

#' @method arrange OTDataSet
#' @export
arrange.OTDataSet <- function(.data, ...) {
  u <- attr(.data, "units")
  x <- nest.OTDataSet(.data)
  dots <- quos(...)
  x <- arrange(x, !!!dots)
  OTDataSet(x[["data"]], x[-length(x)], u)
}

#' @method filter OTDataSet
#' @export
filter.OTDataSet <- function(.data, ..., .preserve = FALSE) {
  u <- attr(.data, "units")
  x <- nest.OTDataSet(.data)
  dots <- quos(...)
  x <- filter(x, !!!dots, .preserve = .preserve)
  if (nrow(x) == 0) {
    stop("None of the transistors satisfies the filtering condition(s)")
  }
  OTDataSet(x[["data"]], x[-length(x)], u)
}

#' @method slice OTDataSet
#' @export
slice.OTDataSet <- function(.data, ..., .preserve = FALSE) {
  u <- attr(.data, "units")
  x <- nest.OTDataSet(.data)
  dots <- quos(...)
  x <- slice(x, !!!dots, .preserve = .preserve)
  if (nrow(x) == 0) {
    stop("None of the transistors is within the range")
  }
  OTDataSet(x[["data"]], x[-length(x)], u)
}

#' @method mutate OTDataSet
#' @export
mutate.OTDataSet <- function(.data, ..., .with = "metadata") {
  dots <- quos(...)
  .with <- match.arg(.with, c("metadata", "IVData"))
  if (.with == "metadata") {
    .data[["metadata"]] <- mutate(.data[["metadata"]], !!!dots)
  } else {
    .data[["IVData"]] <- lapply(.data[["IVData"]], mutate, !!!dots)
  }
  validate_OTDataSet(.data)
}

#' @method select OTDataSet
#' @export
select.OTDataSet <- function(.data, ..., .with = "metadata") {
  dots <- quos(...)
  .with <- match.arg(.with, c("metadata", "IVData"))
  if (.with == "metadata") {
    req_meta <- var_meta()
    .data[["metadata"]] <- select(.data[["metadata"]], !!!dots)
    if (!all(hasName(.data[["metadata"]], req_meta))) {
      stop(
        paste0(
          "Selected columns should include ",
          paste0(req_meta, collapse = ", "),
          " for metadata"
        )
      )
    }
  } else {
    req_iv <- var_iv()
    .data[["IVData"]] <- lapply(.data[["IVData"]], select, !!!dots)
    if (!all(hasName(.data[["IVData"]][[1]], req_iv))) {
      stop(
        paste0(
          "Selected columns should include ",
          paste0(req_iv, collapse = ", "),
          " for IVData"
        )
      )
    }
  }
  validate_OTDataSet(.data)
}

#' @method distinct OTDataSet
#' @export
distinct.OTDataSet <- function(.data, ...) {
  x <- .data[["metadata"]]
  dots <- quos(...)
  distinct(x, !!!dots)
}

#' @method count OTDataSet
#' @export
count.OTDataSet <- function(x, ...) {
  y <- x[["metadata"]]
  dots <- quos(...)
  count(y, !!!dots)
}

# OTAnalysis ---------------------------------------------------------------

#' @method nest OTAnalysis
#' @export
nest.OTAnalysis <- function(.data, ...) {
  m <- .data[["metadata"]]
  iv <- .data[["IVData"]]
  swp <- .data[["sweepResult"]]$sweepData
  qc <- .data[["sweepResult"]]$QC
  est <- .data[["sweepResult"]]$estimate
  otres <- .data[["OTResult"]]

  tibble(
    m, data = iv, sweepData = swp, QC = qc, estimate = est, OTResult = otres
  )
}

#' @method as.data.frame OTAnalysis
#' @export
as.data.frame.OTAnalysis <- function(x, ..., with = "sweepData", .id = TRUE) {
  with <- match.arg(with, c("sweepData", "QC", "estimate", "IVData", "OTResult"))
  if (with == "IVData") {
    y <- nest.OTDataSet(x)
  } else if (with == "OTResult") {
    y <- tibble(x[["metadata"]], data = x[["OTResult"]])
  } else {
    y <- tibble(x[["metadata"]], data = x[["sweepResult"]][[with]])
  }
  if (.id) {
    y$.id <- 1:nrow(y)
  }
  unnest(y, .data$data)
}

#' @method arrange OTAnalysis
#' @export
arrange.OTAnalysis <- function(.data, ...) {
  u <- attr(.data, "units")
  x <- nest.OTAnalysis(.data)
  dots <- quos(...)
  x <- arrange(x, !!!dots)
  sres <- list(sweepData = x[["sweepData"]], QC = x[["QC"]], estimate = x[["estimate"]])
  ota <- new_OTAnalysis(x[["data"]], x[, !sapply(x, is.list)], sres, x[["OTResult"]], units = u)
  validate_OTAnalysis(ota)
}

#' @method filter OTAnalysis
#' @export
filter.OTAnalysis <- function(.data, ..., .preserve = FALSE) {
  u <- attr(.data, "units")
  x <- nest.OTAnalysis(.data)
  dots <- quos(...)
  x <- filter(x, !!!dots, .preserve = .preserve)
  if (nrow(x) == 0) {
    stop("None of the transistors satisfies the filtering condition(s)")
  }
  sres <- list(sweepData = x[["sweepData"]], QC = x[["QC"]], estimate = x[["estimate"]])
  ota <- new_OTAnalysis(x[["data"]], x[, !sapply(x, is.list)], sres, x[["OTResult"]], units = u)
  validate_OTAnalysis(ota)
}

#' @method slice OTAnalysis
#' @export
slice.OTAnalysis <- function(.data, ..., .preserve = FALSE) {
  u <- attr(.data, "units")
  x <- nest.OTAnalysis(.data)
  dots <- quos(...)
  x <- slice(x, !!!dots, .preserve = .preserve)
  if (nrow(x) == 0) {
    stop("None of the transistors is within the range")
  }
  sres <- list(sweepData = x[["sweepData"]], QC = x[["QC"]], estimate = x[["estimate"]])
  ota <- new_OTAnalysis(x[["data"]], x[, !sapply(x, is.list)], sres, x[["OTResult"]], units = u)
  validate_OTAnalysis(ota)
}

#' @method mutate OTAnalysis
#' @export
mutate.OTAnalysis <- function(.data, ..., .with = "metadata") {
  u <- attr(.data, "units")
  dots <- quos(...)
  .with <- match.arg(.with, c("metadata", "IVData"))
  if (.with == "metadata") {
    .data[["metadata"]] <- mutate(.data[["metadata"]], !!!dots)
  } else {
    .data[["IVData"]] <- lapply(.data[["IVData"]], mutate, !!!dots)
  }
  validate_OTAnalysis(.data)
}

#' @method select OTAnalysis
#' @export
select.OTAnalysis <- function(.data, ..., .with = "metadata") {
  dots <- quos(...)
  .with <- match.arg(.with, c("metadata", "IVData"))
  if (.with == "metadata") {
    req_meta <- var_meta2()
    .data[["metadata"]] <- select(.data[["metadata"]], !!!dots)
    if (!all(hasName(.data[["metadata"]], req_meta))) {
      stop(
        paste0(
          "Selected columns should include ",
          paste0(req_meta, collapse = ", "),
          " for metadata"
        )
      )
    }
  } else {
    req_iv <- var_iv()
    .data[["IVData"]] <- lapply(.data[["IVData"]], select, !!!dots)
    if (!all(hasName(.data[["IVData"]][[1]], req_iv))) {
      stop(
        paste0(
          "Selected columns should include ",
          paste0(req_iv, collapse = ", "),
          " for IVData"
        )
      )
    }
  }
  u <- attr(.data, "units")
  ota <- new_OTAnalysis(
    .data[["IVData"]],
    .data[["metadata"]],
    .data[["sweepResult"]],
    .data[["OTResult"]],
    units = u
  )
  validate_OTAnalysis(ota)
}
