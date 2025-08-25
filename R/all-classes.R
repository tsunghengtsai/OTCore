#' OTDataSet constructor and validator
#'
#' @description
#' `OTDataSet` is a class used to restore current-voltage (I-V) measurement
#' data from organic transistors and their metadata. The `OTDataSet` class is a
#' list of two elements named `IVData` and `metadata`.
#'
#' `new_OTDataSet()` constructs an `OTDataSet` object, and
#' `validate_OTDataSet()` checks an `OTDataSet` for expected format and info.
#' `OTDataSet()` is a helper function that wraps the constructor and validator.
#'
#' @param IVData A list of data frames, each restores measurements for one
#' transistor in three or more columns: `V_gate` (gate voltage),
#' `I_drain` (drain current), `I_gate` (gate voltage). The gate voltage sweeps
#' from low to high values (forward sweep), then back from high to low values
#' (backward sweep).
#' @param metadata A `data.frame` that restores the metadata such as substrate
#' material and geometry measurement for each transistor. Each row of the
#' `data.frame` represents a transistor, following the same order as in the
#' list of `IVData`. The following information should be provided in the
#' columns of `metadata`: `substrate` (character), `length` (numeric),
#' `width` (numeric), `thickness` (numeric), and `type` (character, "p" or "n").
#' @param units The unit for measurements of transistor's size ("mm", "micron",
#' or "nm")
#'
#' @returns An OTDataSet object.
#'
#' @aliases OTDataSet OTDataSet-class new_OTDataSet validate_OTDataSet
#'
#' @docType class
#'
#' @rdname OTDataSet
#' @export
#'
#' @examples
#' v <- c(seq(0, 0.5, by = 0.05), seq(0.45, 0, by = -0.05))
#' id1 <- 10^(-5) * v + rnorm(length(v), 0, 10^(-9))
#' id2 <- 10^(-5) * v + rnorm(length(v), 0, 10^(-9))
#' ig1 <- runif(length(v), 0, 10^(-9))
#' ig2 <- runif(length(v), 0, 10^(-9))
#' df1 <- data.frame(I_drain = id1, I_gate = ig1, V_gate = v)
#' df2 <- data.frame(I_drain = id2, I_gate = ig2, V_gate = v)
#' iv <- list(df1, df2)
#' meta <- data.frame(
#'   substrate = rep("A", 2),
#'   length = rep(10, 2),
#'   width = rep(5, 2),
#'   thickness = rep(0.1, 2),
#'   type = rep("p", 2)
#' )
#' ot <- OTDataSet(IVData = iv, metadata = meta)
new_OTDataSet <- function(IVData = list(), metadata = data.frame(), units = "micron") {
  stopifnot(is.list(IVData))
  units <- match.arg(units, c("mm", "micron", "nm"))

  structure(
    list(IVData = IVData, metadata = metadata),
    class = "OTDataSet",
    units = units
  )
}

#' `validate_OTDataSet()` checks an OTDataSet for expected format and info.
#'
#' @param x An OTDataSet
#'
#' @rdname OTDataSet
#' @export
validate_OTDataSet <- function(x) {
  if (!all(utils::hasName(x, c("IVData", "metadata")))) {
    stop("OTDataSet should contain IVData and metadata", call. = FALSE)
  }
  iv <- x[["IVData"]]
  meta <- x[["metadata"]]

  if (length(iv) == 0) {
    stop("I-V data must be provied", call. = FALSE)
  }

  if (!all(sapply(iv, is.data.frame))) {
    stop("I-V data must be provided as a list of data frames", call. = FALSE)
  }

  if (!is.data.frame(meta)) {
    stop("Metadata must be provided as a data frame", call. = FALSE)
  }

  if (length(iv) != nrow(meta)) {
    stop(
      "All I-V data should be provided with their corresponding annotations",
      call. = FALSE
    )
  }

  if (is.null(units)) {
    stop("The unit for dimension measurements should be provided", call. = FALSE)
  }

  # Check required columns in I-V data
  req_iv <- c("I_drain", "I_gate", "V_gate")
  name_iv <- lapply(iv, utils::hasName, req_iv)
  allname_iv <- sapply(name_iv, all)
  if (!all(allname_iv)) {
    err_iv <- paste0(which(!allname_iv), collapse = ", ")
    stop(
      paste0("I-V data indexed ", err_iv, " do not include required columns `I_drain`, `I_gate`, `V_gate`"),
      call. = FALSE
    )
  }

  # Check required columns in metadata
  req_meta <- c("substrate", "length", "width", "thickness", "type")
  if (!all(utils::hasName(meta, req_meta))) {
    stop(
      "Metadata should include `substrate`, `length`, `width`, `thickness`, `type`",
      call. = FALSE
    )
  }

  if (!all(sapply(meta[, c("length", "width", "thickness")], is.numeric))) {
    stop("Numeric values are required for `length`, `width`, `thickness`", call. = FALSE)
  }

  x
}


#' Helper function for `OTDataSet`.
#'
#' @rdname OTDataSet
#' @export
OTDataSet <- function(IVData = list(), metadata = data.frame(), units = "micron") {
  IVData <- as.list(IVData)
  metadata <- as.data.frame(metadata)

  validate_OTDataSet(new_OTDataSet(IVData, metadata, units))
}

#' Create OTDataSet from CSV or Excel files
#'
#' @description
#' `OTDataSet_from_files()` is a helper to load data from CSV or Excel files,
#' construct and then validate an OTDataSet object. `filepath` is a data frame
#' with columns `path` (required), and `sheet` (required, if an Excel file is
#' provided)
#'
#' @param filepath A `data.frame` that provides the information of paths to I-V
#' measurement data for the transistors to be analyzed. It should contain
#' columns `path` (required), and `sheet` (required, if an Excel file is
#' provided).
#' @param metadata A `data.frame` that restores the metadata such as substrate
#' material and geometry measurement for each transistor. Each row of the
#' `data.frame` represents a transistor, following the same order as in the
#' list of `IVData`. The following information should be provided in the
#' columns of `metadata`: `substrate` (character), `length` (numeric),
#' `width` (numeric), `thickness` (numeric), and `type` (character, "p" or "n").
#' @param required_iv A named vector to specify the column names in the data
#' files for the required I-V variables: `I_drain`, `I_gate`, `V_gate`.
#' @param units The unit for measurements of transistor's size ("mm", "micron",
#' or "nm").
#'
#' @returns An OTDataSet object.
#'
#' @export
#' @examples
#' # When I_drain, I_gate, and V_gate are called DrainI, GateI, and GateV,
#' # respectively, in the data files, we specify the correspondence as
#' required_iv <- c(DrainI = "I_drain", GateI = "I_gate", GateV = "V_gate")
OTDataSet_from_files <- function(
    filepath = NULL,
    metadata = NULL,
    required_iv = NULL,
    units = "micron"
) {
  if (is.null(filepath) || !is.data.frame(filepath)) {
    stop("File paths must be provided in `filepath` as a data frame")
  }

  if (is.null(metadata) || !is.data.frame(metadata)) {
    stop("Metadata should be provided in `metadata` as a data frame")
  }

  if (nrow(filepath) != nrow(metadata)) {
    stop("Each data file should have a corresponding annotation in `metadata`")
  }

  if (!utils::hasName(filepath, "path")) {
    stop("Column `path` is required in the data frame `filepath`")
  }

  if (any(is.null(filepath$path) | is.na(filepath$path) | !is.character(filepath$path))) {
    stop("Values in `path` should be character and cannot be NA or NULL")
  }

  # Trim white space
  filepath$path <- trimws(filepath$path)

  # File extensions
  ext <- tolower(sapply(filepath$path, extract_extension))
  if (!all(ext %in% c("csv", "xls", "xlsx"))) {
    stop("Only the following data formats are supported: `.csv`, `.xls`, `.xlxs`")
  }
  filepath$ext <- ext

  if (any(ext %in% c("xls", "xlsx"))) {
    if (!utils::hasName(filepath, "sheet")) {
      stop("Excel files are expected. Column `sheet` is in `filepath`")
    }
    is_excel <- (ext %in% c("xls", "xlsx"))
    if (any(is.na(filepath$sheet[is_excel])) || any(is.null(filepath$sheet[is_excel]))) {
      stop("Values in column `sheet` cannot be NA or NULL for Excel files")
    }
  }

  # Check required columns in I-V data
  if (is.null(required_iv)) {
    required_iv <- c(DrainI = "I_drain", GateI = "I_gate", GateV = "V_gate")
  } else {
    if (!all(c("I_drain", "I_gate", "V_gate") %in% required_iv)) {
      stop("Required I-V names should be specified in required_iv", call. = FALSE)
    }
  }

  files <- vector("list", nrow(filepath))
  for (i in seq_along(files)) {
    if (is_excel[i]) {
      df <- readxl::read_excel(filepath$path[i], sheet = filepath$sheet[i])
    } else {
      df <- readr::read_csv(filepath$path[i])
    }
    if (!all(utils::hasName(df, names(required_iv)))) {
      stop(
        paste0("I-V data indexed ", i, " do not include specified columns",
               " including I_drain, I_gate, and V_gate"),
        call. = FALSE
      )
    }
    df <- df[, names(required_iv)]
    names(df) <- required_iv[names(df)]
    files[[i]] <- df
  }

  OTDataSet(IVData = files, metadata = metadata, units = units)
}

#' OTAnalysis constructor and validator
#'
#' @description
#' `OTAnalysis` is an extended class of `OTDataSet` that includes extra details
#' about analysis results summarized at the sweep-level and transistor-level.
#' `new_OTAnalysis()` constructs an `OTAnalysis` object, and
#' `validate_OTAnalysis()` checks an `OTAnalysis` for expected format and info.
#'
#' The constructor (and validator) would not typically be used by an
#' end user. These functions are called by the `OT_analyze()` function that
#' takes as input an `OTDataSet` and returns a new `OTAnalysis` object. They may
#' also be called by methods for modifying an `OTAnalysis` object.
#'
#' @param IVData A list of data frames, each restores measurements for one
#' transistor in three or more columns: `V_gate` (gate voltage),
#' `I_drain` (drain current), `I_gate` (gate voltage). The gate voltage sweeps
#' from low to high values (forward sweep), then back from high to low values
#' (backward sweep).
#' @param metadata A `data.frame` that restores the metadata such as substrate
#' material and geometry measurement for each transistor. Each row of the
#' `data.frame` represents a transistor, following the same order as in the
#' list of `IVData`. As in `OTDataSet`, the following information should be
#' provided: `substrate` (character), `length` (numeric), `width` (numeric),
#' `thickness` (numeric), and `type` (character, "p" or "n"). Based on the
#' dimension measurements, a new column `dim_ratio` (numeric) is added.
#' @param sweepResult A list of three lists of the same length as the number of
#' transistors. Each of the three lists contains data frames for sweep-level
#' information about the transistors. The `sweepData` list consists of data
#' frames augmented from those in `IVData`, with information about sweep and
#' calculated transconductance. The `QC` list provides quality measures and
#' corresponding outlyingness in terms of maximum drain current value,
#' difference between drain and gate currents, and on-off ratio and range.
#' The `estimate` list provides sweep-level summaries in terms of outlyingness,
#' maximum transconductance, threshold voltage, and area under the transfer
#' curve.
#' @param OTResult A list of data frames, each summarizes the analysis in terms
#' of outlyingness, average maximum transconductance, difference in threshold
#' voltage between sweeps, and difference area under the transfer curve.
#' @param units The unit for measurements of transistor's size ("mm", "micron",
#' or "nm")
#'
#' @returns An OTAnalysis object.
#'
#' @aliases OTAnalysis OTAnalysis-class new_OTAnalysis validate_OTAnalysis
#'
#' @docType class
#'
#' @rdname OTAnalysis
#' @export
new_OTAnalysis <- function(
    IVData = list(),
    metadata = data.frame(),
    sweepResult = list(),
    OTResult = list(),
    units = "micron"
) {
  stopifnot(is.list(IVData))
  stopifnot(is.list(sweepResult))
  units <- match.arg(units, c("mm", "micron", "nm"))

  structure(
    list(IVData = IVData, metadata = metadata, sweepResult = sweepResult, OTResult = OTResult),
    class = c("OTAnalysis", "OTDataSet"),
    units = units
  )
}

#' `validate_OTAnalysis()` checks an OTAnalysis for expected format and info.
#'
#' @param x An OTAnalysis object
#'
#' @rdname OTAnalysis
#' @export
validate_OTAnalysis <- function(x) {
  if (!all(utils::hasName(x, c("IVData", "metadata", "sweepResult", "OTResult")))) {
    stop(
      "OTAnalysis should contain `IVData`, `metadata`, `sweepResult`, `OTResult`",
      call. = FALSE
    )
  }
  validate_OTDataSet(x)

  iv <- x[["IVData"]]
  meta <- x[["metadata"]]
  sres <- x[["sweepResult"]]
  otres <- x[["OTResult"]]

  if (!all(utils::hasName(sres, c("sweepData", "QC", "estimate")))) {
    stop("sweepResult is a list of `sweepData`, `QC`, and `estimate` ", call. = FALSE)
  }

  if (!all(sapply(sres, length) == nrow(meta))) {
    stop("sweepData, QC, estimate must all have the same length as IVData", call. = FALSE)
  }

  if (length(otres) != nrow(meta)) {
    stop("OTResult should have the same length as IVData", call. = FALSE)
  }

  x
}
